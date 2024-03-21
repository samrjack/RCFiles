#!/bin/bash

if [ ! -f "${HOME}/.emacs.d/bin/doom" ]
then
    git clone https://github.com/hlissner/doom-emacs "${HOME}/.config/emacs"
    "${HOME}"/.config/emacs/bin/doom install

    # Make directory for personal dictionary
    mkdir -p ${HOME}/.emacs.d/.local/etc/ispell
    echo "personal_ws-1.1 en 0" > ${HOME}/.emacs.d/.local/etc/ispell/en.pws
fi
