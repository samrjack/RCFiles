#!/bin/bash

if [ ! -f "${HOME}/.emacs.d/bin/doom" ]
then
    git clone https://github.com/hlissner/doom-emacs "${HOME}/.emacs.d"
    "${HOME}"/.emacs.d/bin/doom install

    # Make directory for personal dictionary
    mkdir -p ${HOME}/.emacs.d/.local/etc/ispell
    touch ${HOME}/.emacs.d/.local/etc/ispell/en.pws
fi
