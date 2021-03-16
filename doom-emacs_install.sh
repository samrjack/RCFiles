#!/bin/bash

if [ ! -f $HOME/.emacs.d/bin/doom ]
then
    git clone --depth 1 https://github.com/hlissner/doom-emacs $HOME/.emacs.d
    $HOME/.emacs.d/bin/doom install
fi
