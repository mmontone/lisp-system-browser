# LISP-SYSTEM-BROWSER

This is Smalltalk-like browser for Common Lisp.

NOTE THAT THIS IS VERY MUCH WORK IN PROGRESS AT THIS MOMENT.

![lisp-system-browser](system-browser.gif "lisp-system-browser screenshot")

## Install

ℹ️ Please consider using [SLIME :star:](https://github.com/mmontone/slime-star), that comes with this extension preinstalled.

Clone this repository using git `--recursive` option, as this repository contains submodules.

Load `swank` and add this repository path to `swank::*load-path*`, in your Lisp compiler init file (~/.sbclrc if using SBCL):

```lisp
(require :swank)
(push #p"~/lisp-system-browser/" swank::*load-path*)
```

In Emacs, add this repository path to `load-path` and `system-browser` to `slime-contribs` in `~/.emacs` init file, like:

```
(push "~/lisp-system-browser" load-path)

(setq slime-contribs '(slime-fancy system-browser))

(slime-setup)
```

Install Emacs dependencies:

This depends on [emacs-window-layout](https://github.com/kiwanami/emacs-window-layout) Emacs library. Appears as `window-layout` in `M-x package-list-packages` (melpa).

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
