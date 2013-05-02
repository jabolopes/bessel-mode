bsl-mode
========

Emacs major mode for the [Bessel](http://github.com/jabolopes/bsl)
programming language.


Installation
------------

Download the file 'bsl-mode.el' and place it in a directory that is
reachable through Emacs.

For example, I will assume that the file 'bsl-mode.el' was placed in
the following directory.

    ~/.emacs.d/site-lisp/bsl-mode

Now, add the following to your '.emacs' to autoload 'bsl-mode' when
files with the extension '.bsl' are open in Emacs.

    ; bsl

    (add-to-list 'load-path "~/.emacs.d/site-lisp/bsl-mode")
    (require 'bsl-mode)