;;; system-idle.el --- Poll the system-wide idle time  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:   Martin Edström <meedstrom91@gmail.com>
;; Created:  2026-01-05
;; Keywords: lisp
;; URL:      https://github.com/meedstrom/system-idle
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Call `system-idle-seconds' to get the number of seconds since last user
;; activity on the computer.

;; This differs from the built-in `current-idle-seconds', which can only be
;; used for that purpose as long as Emacs is "in focus".

;; This differs from `org-user-idle-seconds' in org-clock.el, by adding
;; support for Wayland.  Hopefully more in the future.  Plus your code won't
;; have to depend on loading Org, just this little library.

;; Supported environments:

;; - Mac OS
;; - Wayland (GNOME)
;; - Wayland (KDE and others supporting ext-idle-notify)
;;   - https://wayland.app/protocols/ext-idle-notify-v1#compositor-support
;;   - Returns 0 if invoked in the first 9 seconds or so
;;   - Requires installing "swayidle"
;; - X11 (GNOME)
;; - X11 (any)
;;   - Requires installing "x11idle" or "xprintidle"

;; To test that it works, eval the following and do not touch the computer for
;; 11 seconds.  A number should be printed to *Messages*.

;;     (run-with-timer 11 nil (lambda () (print (system-idle-seconds))))

;; A known issue: `system-idle-seconds' is not guaranteed to return correct
;; results if user has switched from the graphical desktop to a TTY console
;; and is in the console by the time it is called.


;;; Code:
;;;; Plumbing:

(require 'seq)
(require 'cl-lib)
(declare-function dbus-list-activatable-names "dbus" (&optional bus))
(declare-function dbus-call-method "dbus" (bus service path interface method &rest args))
(declare-function dbus-get-property "dbus" (bus service path interface property))

;;;;; MacOS:

(defun system-idle--poll-mac-alt ()
  "Copy of `org-mac-idle-seconds'."
  (string-to-number
   (shell-command-to-string
    "ioreg -c IOHIDSystem | perl -ane 'if (/Idle/) {$idle=(pop @F)/1000000000; print $idle; last}'")))

;; https://old.reddit.com/r/emacs/comments/1qzj8bh/a_little_library_systemidle/o4h5qh8/
(defconst system-idle--poll-mac-re (rx "HIDIdleTime" (* (not digit)) (group (+ digit))))
(defun system-idle--poll-mac ()
  "Get idle time on MacOS."
  (let ((match (save-match-data
                 (with-temp-buffer
                   (when (eq 0 (call-process "ioreg" nil t nil "-r" "-k" "HIDIdleTime"))
                     (goto-char (point-min))
                     (when (re-search-forward system-idle--poll-mac-re nil t)
                       (match-string 1)))))))
    (if match
        (/ (string-to-number match)
           (float 1e9))
      (setq system-idle-seconds-function #'system-idle--poll-mac-alt)
      (system-idle--poll-mac-alt))))

;;;;; x11idle:

(defvar system-idle--x11-program nil)
(defun system-idle--poll-x11 ()
  "Like `org-x11-idle-seconds'."
  (unless system-idle--x11-program
    (error "system-idle-seconds-x11: Install x11idle or xprintidle"))
  (with-temp-buffer
    (if (eq 0 (call-process system-idle--x11-program nil t))
        (/ (string-to-number (buffer-string))
           (float 1e3))
      (error "system-idle-seconds-x11: %s failed: %s"
             system-idle--x11-program
             (buffer-string)))))

;;;;; elogind (and systemd-logind?):

(defvar system-idle--dbus-session-path nil
  "Copy of `org-logind-dbus-session-path'.")

(defun system-idle--poll-logind ()
  "Copy of `org-logind-user-idle-seconds'."
  (- (float-time)
     (/ (dbus-get-property :system
                           "org.freedesktop.login1"
                           system-idle--dbus-session-path
                           "org.freedesktop.login1.Session"
                           "IdleSinceHint")
        (float 1e6))))

;;;;; GNOME:

;; https://unix.stackexchange.com/questions/396911/how-can-i-tell-if-a-user-is-idle-in-wayland
(defun system-idle--poll-gnome ()
  "Check Mutter\\='s idea of idle time, even on Wayland."
  (let* ((output (with-temp-buffer
                   (call-process "dbus-send" nil t nil
                                 "--print-reply"
                                 "--dest=org.gnome.Mutter.IdleMonitor"
                                 "/org/gnome/Mutter/IdleMonitor/Core"
                                 "org.gnome.Mutter.IdleMonitor.GetIdletime")
                   (buffer-string)))
         (idle-ms (if (string-match (rx space (+ digit) eol) output)
                      (string-to-number (match-string 0 output))
                    (error "system-idle--poll-gnome: Broken, did GNOME change API?"))))
    (/ idle-ms (float 1e3))))

;;;;; Swayidle:

(defvar system-idle--swayidle-process nil)
(defvar system-idle--touch-me-file
  (expand-file-name "emacs-system-idle" temporary-file-directory))

(defun system-idle--ensure-swayidle (&optional restart)
  "Ensure that our Swayidle instance is running.
RESTART means restart it."
  (when (and restart (processp system-idle--swayidle-process))
    (delete-process system-idle--swayidle-process))
  (when (not (process-live-p system-idle--swayidle-process))
    (setq system-idle--swayidle-process
          (let ((default-directory temporary-file-directory))
            (start-process-shell-command
             "system-idle-swayidle"
             " *system-idle-swayidle*"
             (format "swayidle timeout 9 'touch %s' resume 'rm -f %s'"
                     (shell-quote-argument system-idle--touch-me-file)
                     (shell-quote-argument system-idle--touch-me-file)))))
    (set-process-query-on-exit-flag system-idle--swayidle-process nil)))

(defun system-idle--poll-swayidle ()
  "Check idle on compositors supporting the ext-idle-notify protocol.
Returns 0 if invoked during the first 9 seconds."
  (system-idle--ensure-swayidle)
  (let ((attr (file-attributes system-idle--touch-me-file)))
    (if attr
        (+ 9 (time-to-seconds
              (time-since (file-attribute-modification-time attr))))
      0)))

(defconst system-idle--swayidle-support-re
  (rx word-boundary
      (or "kwin" ; Verified 2026-02-07
          ;; Not verified, just guessing from
          ;; https://wayland.app/protocols/ext-idle-notify-v1#compositor-support
          "cage" "cosmic" "hyprland" "jay"  "labwc" "louvre"
          "niri" "phoc" "river" "sway" "treeland" "wayfire")
      word-boundary)
  "Regexp matching process invocation string of a supported compositor.")

;; REVIEW: If we decide `system-idle--swayidle-support-re' is too sloppy a
;; method, maybe use XDG_CURRENT_DESKTOP:
;; https://unix.stackexchange.com/a/645761
;; But:
;; https://old.reddit.com/r/swaywm/comments/vy4lrr/why_doesnt_sway_set_xdg_current_desktop/
(defun system-idle--swayidle-supported-p ()
  "Return t if the running compositor supports ext-idle-notify."
  (cl-loop
   for args in (mapcar (lambda (pid)
                         (alist-get 'args (process-attributes pid)))
                       (list-system-processes))
   when (and args (string-match-p system-idle--swayidle-support-re args))
   return t))


;;;; Porcelain:

(defvar system-idle-seconds-function nil
  "Function to be invoked by `system-idle-seconds', must return a number.")

(defun system-idle-seconds ()
  "Return the number of seconds the system has been idle.

Unlike `current-idle-time', the result is intended to be correct even
while Emacs is not in focus, i.e. to return a value close to 0 if the
user is still operating the computer but has a web browser or other
application in focus.

Unlike `current-idle-time', always returns a floating-point number and
never returns nil.

If `system-idle-seconds-function' is `system-idle--poll-swayidle',
returns 0.0 for the first ~9 seconds of idle."
  (unless system-idle-seconds-function
    (system-idle-reconfigure t))
  (let ((value (funcall system-idle-seconds-function)))
    (if (numberp value)
        (float value)
      (error "Function system-idle-seconds-function did not return a number"))))

(defun system-idle-reconfigure (&optional assert)
  "Try to set `system-idle-seconds-function' and return non-nil.
Upon failure, signal an informative error if ASSERT, else return nil."
  (setq system-idle--x11-program
        (seq-find #'executable-find '("x11idle" "xprintidle")))
  (setq system-idle--dbus-session-path
        (when (and (boundp 'dbus-runtime-version)
                   (require 'dbus nil t)
                   (member "org.freedesktop.login1" (dbus-list-activatable-names)))
          (ignore-errors
            (dbus-call-method :system
                              "org.freedesktop.login1"
                              "/org/freedesktop/login1"
                              "org.freedesktop.login1.Manager"
                              "GetSessionByPID"
                              (emacs-pid)))))
  (setq system-idle-seconds-function
        (or (and (eq system-type 'darwin)
                 #'system-idle--poll-mac)
            (let ((DESKTOP_SESSION (getenv "DESKTOP_SESSION")))
              (and DESKTOP_SESSION
                   (string-match-p (rx word-boundary (or "gnome" "ubuntu"))
                                   DESKTOP_SESSION)
                   (if (ignore-errors (system-idle--poll-gnome))
                       #'system-idle--poll-gnome
                     (when assert
                       (system-idle--poll-gnome)))))
            (and (not (memq system-type '(ms-dos windows-nt cygwin haiku android)))
                 (system-idle--swayidle-supported-p)
                 (if (executable-find "swayidle")
                     (progn
                       (system-idle--ensure-swayidle t)
                       #'system-idle--poll-swayidle)
                   (when assert
                     (error "system-idle: Install swayidle"))))
            (and system-idle--dbus-session-path
                 #'system-idle--poll-logind)
            ;; NOTE: This condition is also true under XWayland, so it must come
            ;; after all other checks for Wayland compositors if we want it to be
            ;; invoked under "native X" only.
            (and (eq window-system 'x)
                 (if system-idle--x11-program
                     #'system-idle--poll-x11
                   (when assert
                     (error "system-idle: Install x11idle or xprintidle"))))
            (when assert
              (error "system-idle: Could not get idle time on this system")))))

;; Make sure to spawn a Swayidle process now at load time if that's what we'll
;; use, so `system-idle-seconds' will return an accurate value on first use.
(unless system-idle-seconds-function
  (system-idle-reconfigure))

(provide 'system-idle)

;;; system-idle.el ends here

;; Local Variables:
;; emacs-lisp-docstring-fill-column: 72
;; End:
