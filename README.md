Call `system-idle-seconds` to get the number of seconds since last user activity on the computer.

This differs from the built-in `current-idle-time`, which can only be used for that purpose as long as Emacs is "in focus".

This differs from `org-user-idle-seconds` in org-clock.el, by adding support for Wayland.  Hopefully more in the future.  Plus your code won't have to depend on loading Org, just this little library.

# Supported environments

- Mac OS
- Wayland (GNOME)
- Wayland (KDE Plasma, and [other compositors](https://wayland.app/protocols/ext-idle-notify-v1#compositor-support) supporting the ext-idle-notify protocol)
    - Returns 0 if invoked in the first 9 seconds or so
    - Requires installing `swayidle`
- X11 (GNOME)
- X11 (any)
    - Requires installing `x11idle` or `xprintidle`

To test that it works, eval the following and do not touch the computer for 11 seconds.  A number should be printed to `*Messages*`.

    (run-with-timer 11 nil (lambda () (print (system-idle-seconds))))


> [!IMPORTANT]
> **Your Help Needed!** It's impossible to test all possible systems, so please [take a moment to report in](https://github.com/meedstrom/system-idle/issues/1) about whether this works on yours or not.
>
> If you're willing to test solutions, even better!  Then we should be able to add support for your system type.

# Known issues

Not guaranteed to return correct results if user has switched from the graphical desktop to a TTY console and is in the console by the time it is called.

