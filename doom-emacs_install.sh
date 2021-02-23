#!/bin/bash

if [ ! -d $HOME/.doom.d ]
then
    git clone --depth 1 https://github.com/hlissner/doom-emacs $HOME/.emacs.d
    $HOME/.emacs.d/bin/doom install
fi
