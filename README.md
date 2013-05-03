bessel-mode
===========

Emacs major mode for the [Bessel](http://github.com/jabolopes/bessel)
programming language.


Installation
------------

Download the file 'bessel-mode.el' and place it in a directory that is
reachable to Emacs.

For example, I will assume that the file 'bessel-mode.el' was placed
in the following directory.

    ~/.emacs.d/site-lisp/bessel-mode

Now, add the following to your '.emacs' to autoload 'bessel-mode' when
files with the extension '.bsl' are open in Emacs.

    ; bessel

    (add-to-list 'load-path "~/.emacs.d/site-lisp/bessel-mode")
    (require 'bessel-mode)