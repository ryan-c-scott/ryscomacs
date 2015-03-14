ryscomacs
=========

My emacs settings, all in one convenient place

# Configuration
The following files are loaded on startup.  Specifically 'localconfig.el' is loaded first and 'localprojects.el' last.
* "~/.emacs.d/elisp/localconfig.el"
* "~/.emacs.d/elisp/localprojects.el"

Buffers that aren't visiting files are hidden from the normal buffer switching lists.
C languages are set to use the stroustrup style for code formatting
.h files are always loaded in c++-mode


# Commands of note
There are several custom configuration options, bindings, etc.

* <CAPSLOCK><SPC> :  Helm mini
* C-x c i :  Semantic mode.  This shows language specific information for the buffer.
* F8 :  Brings up a file browser interface
* helm-colors :  Brings up a helm interface for color selection.
* C-= :  Expands the current selection/point by semantic units (e.g. clause, function, class, etc.)

## Window Manipulation
Ryscomacs is designed to utilize the CAPSLOCK key for doing window splitting.

Manipulation:

* <CAPSLOCK><CAPSLOCK> :  Switch buffer
* <CAPSLOCK><DOWN> :  Split window horizontally
* <CAPSLOCK><RIGHT> :  Split window vertically
* <CAPSLOCK><UP> :  Close other windows
* <CAPSLOCK><LEFT> :  Close this window

Movement between windows can be done with the META key in conjunction with the arrows.

### Capslock Overriding
On Windows machines, Emacs provides a configuration option for disabling the OS's default handling of CAPSLOCK.  However, on any other OS it must be mapped to F12 at the OS level and the variable 'rysco-capslock-mapped' set in your localconfig.

If you ssh into other machines and use Emacs remotely, you'll want to map F12 as you would on other systems.

Mapping at the OS level is generally easiest by way of utilities that are readily available.  For windows, two registry files for setting and unsetting that binding can be found under 'win32'.

## Version Control
Between DVC and VC most, if not all, of your source control needs can be met.

* dvc-diff provides a list of modified/added/removed files followed by a unified diff for everything
* 'm' marks files in that list to be operated on
* 'c' brings up a new buffer for writing a commit message
* 'C-c C-c' will commit the marked files with the authored message

Files that have not been added yet must be marked and added with 'a' prior to commit.

* dvc-log will show the log for the current branch

Under VC (e.g. when using SVN), vc-root-diff will give you a unified diff of the project. vc-diff works only from the current directory down.

## Custom Commands
* json-format
* json-unformat
* insert-standard-date :  Inserts the current date
* vertical-windows-with-related:  Splits the window vertically and loads the related file (e.g. the header for a C file) in the other window
* edit-local-projects :  Opens/creates the localprojects.el file
* edit-local-config :  Opens/creates the localconfig.el file

# Included Sources
* cg-mode
* color-theme [README](elisp/color-theme-6.6.0/README)
* csharp-mode
* dvc
* erlang-mode
* expand-region [README](elisp/expand-region/README.md)
* glsl-mode
* helm-ag
* helm
* json-mode
* lua-mode
* markdown-mode
* nav [README](elisp/nav/README.md)
* p4
* php-mode
* protobuf-mode
