# Init Directory (instead of a single file)

Keep all Emacs configuration in a separate directory that can be under
version control with very short init.el change.  Using init-dir is as
simple as putting the following single line in the actual init.el
file:

``` emacs-lisp
(load-init-dir)
```

## Why init-dir?

There are already a few other init directory packages commonly
used. The most popular ones are
[el-init](https://github.com/HKey/el-init) and
[init-loader](https://github.com/emacs-jp/init-loader). What
distinguishes init-dir from these other packages?

* No constraints put on how you write your init directory. Other
  packages require you to follow naming conventions or use `provide` /
  `require`.
* Integration testing recent Emacs versions while keeping support for
  older Emacs versions. I've been using this package myself since
  Emacs 21.3 across Mac, Windows, and Debian Linux and I understand
  the entire ecosystem can't update all at once.
* Avoid duplicating built-in Emacs functionality. For example in all
  elisp code you can use the form `(when (eq system-type 'windows-nt)
  ...)` to run code only on Windows so init-dir doesn't have any
  special "only runs on Windows" functionality.

## Installation

The easiest way to install init-dir is on GNU Emacs 29 via
`package-vc-install`. Just run `M-x package-vc-install RET
http://github.com/chaosemer/init-dir RET`. This installs the package
locally. (Very soon this package will be available on MELPA and MELPA
Stable as well.)

## Init directory organizational suggestions

I find it easiest to organize my [init
directory](https://github.com/chaosemer/dot-emacs) by mode or
package. So I have c.el for C related configuration (including C++,
C#, etc.), elisp.el for Emacs Lisp related configuration, window.el
for window management utilities, and so on. Each of these
configuration files is independent from each other and self contained
making it easy to add keep things organized.

For configurations that apply to all of Emacs, I find it best to put
it in a single file named emacs.el. This is where global minor modes
and global keybindings go.

Take advantage of the files being loaded in lexicographic order!
Utility functions that should be globally available can be put in
files named something like 00.required-libraries.el so it gets loaded
first.

## History

init-dir was originally written by Jared Finder (<jared@finder.org>)
back in 2005 for GNU Emacs 21.3. My goals were:

* Keep my Emacs customizations under version control on a platform
  that didn't support symbolic links (Windows 2000). At the time GNU
  Emacs always loaded customizations from ~/.emacs.
* Keep global customizations cleanly separated from programming
  language specific customizations.
* Keep work customizations that depended on confidential tooling out
  of version control.

init-dir was publicly shared years later in 2023 based on
encouragement from Eliza (<https://github.com/elizagamedev>) to share
all the helpful Emacs utilities I have built.
