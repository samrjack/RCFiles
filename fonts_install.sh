#!/bin/bash
# Installs all the nerd fonts

install_location=$HOME/.fonts

mkdir -p $install_location
if [ ! -d $install_location/nerd-fonts/ ]; then
    git clone https://github.com/ryanoasis/nerd-fonts $install_location/nerd-fonts
fi
