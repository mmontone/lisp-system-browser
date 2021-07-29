# LISP-SYSTEM-BROWSER

This is Smalltalk-like browser for Common Lisp.

NOTE THAT THIS IS VERY MUCH WORK IN PROGRESS AT THIS MOMENT.

![lisp-system-browser](system-browser.gif "lisp-system-browser screenshot")

## Install

Load `swank` and add this repository path to `swank::*load-path*`, in your Lisp compiler init file (~/.sbclrc if using SBCL):

```lisp
(require :swank)
(push #p"/home/marian/src/lisp/lisp-system-browser/" swank::*load-path*)
```

In Emacs, add this repository path to `load-path` and `system-browser` to `slime-contribs` in `~/.emacs` init file, like:

```
(push "/home/marian/src/lisp/lisp-system-browser" load-path)

(setq slime-contribs '(slime-fancy system-browser))

(slime-setup)
```

## Use

`M-x lisp-system-browser` to open the browser.

### Commands

```
lisp-system-browser	      M-x ... RET
   Open the Common Lisp system browser.
quit-system-browser	      M-x ... RET
   Quit the system browser.
system-browser		      M-x ... RET
   Open the currently instantiated system browser.
system-browser-refresh	      M-x ... RET
   Refresh the system browser contents and reset its layout.
system-browser-reset-layout   M-x ... RET
   Reset system browser layout. Use this when Emacs windows break the
   browser's layout.
system-browser-toggle-docs    M-x ... RET
   Toggle documentation panel in system browser.
system-browser-browse-package M-x ... RET
   Browse a particular package completed from command bar.
```
