ryscomacs
=========

My emacs settings, all in one convenient place

# Configuration
'localconfig.el' is loaded during startup to provide a place for installation specific configuration.

Buffers that aren't visiting files are hidden from the normal buffer switching lists.
C languages are set to use the stroustrup style for code formatting
.h files are always loaded in c++-mode

## Installation
If you are bootstrapping a truly empty Emacs setup, you can copy `example/dotEmacs.el` to `~/.emacs` and you're all set.  That file also has the `straight.el` and accompanying `org-mode` hacks that all the kids are raving about these days.

The following snippet will pull down the latest and get you setup

``` shell
cd ~
wget -O .emacs https://bitbucket.org/scott105/ryscomacs/raw/master/example/dotEmacs.el
```

If you are integrating into an existing emacs configuration
``` emacs-lisp
(straight-use-package
 '(ryscomacs :type git :repo "git@bitbucket.org:scott105/ryscomacs.git"))
(require 'rysco-core)
```

Fonts can be set in your `localconfig.el` by setting `rysco-font` and `rysco-font-size`
``` emacs-lisp
(setq rysco-font "Source Code Pro")
(setq rysco-font-size "12.0")
```

If you would like to use a fancy SVG modeline instead of `powerline`:
``` emacs-lisp
(setq rysco-fancy-modeline t)
(setq rysco-fancy-modeline-theme 'ocodo-minimal-light-smt)
```

Changing `rysco-fancy-modeline-theme` as appropriate.

## Lead Key
Ryscomacs is designed to utilize a lead key for doing window splitting and other frequently used commands.  This can help to disambiguate special ryscomacs functionality from modes or emacs in general.

`rysco-lead-key` is by default set to `<escape>`

# Commands of note
There are several custom configuration options, bindings, etc.

* <LEAD><LEAD> :  Switch buffer (helm-mini)
* <LEAD><SPC> :  Semantic mode.  This shows language specific information for the buffer.
* <LEAD>s :  Helm occur
* <LEAD><RET> :  Helm resume session
* <LEAD>p : Launch Magit (defaulting to current directory)
* <LEAD>w : Run an ag helm search (actually RipGrep) from the root of the Projectile project
* helm-colors :  Brings up a helm interface for color selection.
* C-c o :  In C modes, opens the related file in a verical pane (using vertical-windows-with-related)

## Window Manipulation
* <LEAD>g : Dismiss current window
* <LEAD>r : Maximize current window
* <LEAD>h : Move left to window; possibly creating it.
* <LEAD>t : Move down to window; possibly creating it.
* <LEAD>n : Move right to window; possibly creating it.
* <LEAD>/ : Create new frame
* <LEAD>= : Destroy current frame
* <LEAD>\ : Cycle to next frame

You may notice that those bindings are on strange keys and it's because they're Dvorak-centric and I'm sorry.
The *"Spiritual intention"* there is `uoijkl`.

## Version Control
Magit and a fork of Monky are provided for working with Git and Mercurial repositories respectively.
`vc-mode` should provide most other things you need, with the most notable exception being P4.

## Custom Commands
* `vertical-windows-with-related`:  Splits the window vertically and loads the related file (e.g. the header for a C file) in the other window
* `edit-local-config`:  Opens/creates the `localconfig.el` file
* `killall`:  Kills all buffers that don't start with '*' and maximizes the scratch buffer

# Included Sources
Many packages are directly included under [elisp/](elisp/).
These packages are not included via a package manager in order to effectively pin their versions to a single point and/or provide a simplified approach to making any necessary modifications.

Specifically `powerline` is of note as affects what you will see in the modeline.
Also included and integral to my own usage, `helm` and `projectile` are included and broadly enabled.

# Extras
## Key remapping (Caps->Ctrl et al)
Mapping at the OS level is generally easiest by way of utilities that are readily available.
For windows, two registry files for setting and unsetting that binding can be found under 'win32'.

## Keyboards
I have keyboard problems. [Readme](extras/keyboard/README.md)

## OSX
Slate can be used to provide some convenient keyboard driven tools for moving windows around.

## Win32
On Windows machines, Emacs provides a configuration option for disabling the OS's default handling of CAPSLOCK.  However, on any other OS it must be mapped to F12 at the OS level and the variable 'rysco-capslock-mapped' set in your localconfig.

## zsh
As an added bonus, the most _extra_ of _extras_, included is my Zshell setup.
It has been tested on windows/cygwin, OSX, and Linux.

Link/copy zshrc to ~/.zshrc and away you go.
