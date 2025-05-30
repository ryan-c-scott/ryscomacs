ryscomacs
=========

My emacs settings, all in one convenient place

# Configuration
'ryscomacs/config.el' is loaded during startup to provide a place for installation specific configuration.  It should be placed under '.emacs.d'.

Buffers that aren't visiting files are hidden from the normal buffer switching lists.
C languages are set to use the stroustrup style for code formatting
.h files are always loaded in c++-mode

## Installation
If you are bootstrapping a truly empty Emacs setup, you can copy `example/init.el` into `~/.emacs.d/` and you're all set.  That file also has the `straight.el` and accompanying `org-mode` hacks that all the kids are raving about these days.

The following snippet will pull down the latest and get you setup

``` shell
mkdir ~/.emacs.d
cd ~/.emacs.d
wget -O init.el https://raw.githubusercontent.com/ryan-c-scott/ryscomacs/master/example/init.el
```

If you are integrating into an existing emacs configuration
``` emacs-lisp
(straight-use-package
 '(ryscomacs :type git :host github :repo "ryan-c-scott/ryscomacs"))
(require 'rysco-core)
```

Fonts can be set in your `ryscomacs/config.el` by setting `rysco-font` and `rysco-font-size`
``` emacs-lisp
(setq rysco-font "Source Code Pro")
(setq rysco-font-size "12.0")
```

Additionally, in Emacs >= 27, you can add `.emacs.d/early-init.el` which will run prior to the GUI being loaded. This can be used to stop the visual artifacts of the stock Emacs window showing up prior to everything that is configured in `.emacs.d/init.el`.

An example file is in `example/early-init.el`.

### Windows Users
In order to support symlink creation, used by Straight.el, on Windows, a security policy must be changed.
To quote Straight.el's documentation:

``` text
Your user-account needs to be assigned the right to create symbolic links. To do so, run "secpol.msc" and in "Local Policies → User Rights Assignment" assign the right to "Create symbolic links" to your user-account.
```

#### Native Compilation
  1. Install msys2 in chocolatey
  2. Run msys2
  3. `pacman -S emacs`
  4. `pacman -S libgccjit`
  5. pin `C:\tools\msys64\mingw64\bin\runemacs.exe` to launch bar

Note: `libgccjit` is only present in the `mingw64` and `ucrt64` msys2 environments.

Running `runemacs` outside of the msys2 environment will make allow Emacs to maintain the same paths to pick up Git and other tools.

```text
(native-comp-available-p)
system-configuration-options
system-configuration-features
```

`native-comp-available-p` will return nil if Emacs hasn't been configured with `--with-native-compilation` /or/ if it can't find `libgccjit-0.dll`

Ensure that `libgccjit-0` is in a place where that particular Emacs binary will load it.

## Lead Key
Ryscomacs is designed to utilize a lead key for doing window splitting and other frequently used commands.  This can help to disambiguate special ryscomacs functionality from modes or emacs in general.

`rysco-lead-key` is by default set to `<escape>`

# Commands of note
There are several custom configuration options, bindings, etc.

* `<LEAD> <LEAD>` :  Switch buffer (helm-mini)
* `<LEAD> <SPC>` :  Semantic mode.  This shows language specific information for the buffer.
* `<LEAD> s` :  Helm occur
* `<LEAD> <RET>` :  Helm resume session
* `<LEAD> p` : Launch Magit (defaulting to current directory)
* `<LEAD> w` : Run an ag helm search (actually RipGrep) from the root of the Projectile project
* helm-colors :  Brings up a helm interface for color selection.
* C-c o :  In C modes, opens the related file in a verical pane (using vertical-windows-with-related)

## Window Manipulation
* `<LEAD> g` : Dismiss current window
* `<LEAD> r` : Maximize current window
* `<LEAD> h` : Move left to window; possibly creating it.
* `<LEAD> t` : Move down to window; possibly creating it.
* `<LEAD> n` : Move right to window; possibly creating it.
* `<LEAD> /` : Create new frame
* `<LEAD> =` : Destroy current frame
* `<LEAD> \` : Cycle to next frame

You may notice that those bindings are on strange keys and it's because they're Dvorak-centric and I'm sorry.
The *"Spiritual intention"* there is `uoijkl`.

## Version Control
Magit is provided for working with Git repositories.
`vc-mode` should provide most other things you need, with the most notable exception being P4.

## Custom Commands
Two built-in "transient" menus are provided and by default bound to `<lead> <lead>` and `<lead> <tab>`.
The main one provides an access point to some of the higher-level features of Ryscomacs.
The second is an empty menu for users to place their items by adding 'transient' compatible suffix commands to the list 'personal-transients' during the processing of 'ryscomacs/config.el'.

# Included Sources
Several packages are directly included.
These packages are not included via a package manager in order to effectively pin their versions to a single point and/or provide a simplified approach to making any necessary modifications.

Some bundled packages are developed as a part of ryscomacs, but may eventually be forked into their own repositories/packages.

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
