#! /bin/bash
# Installs vundle for managing vim packages

### Vundle install ###

vundleDirectory="$HOME/.vim/bundle";
if [ ! -d $vundleDirectory ]; then
    mkdir -vp $vundleDirectory;
fi

vundleName="Vundle.vim";
vundlePath="$vundleDirectory/$vundleName"
if [ ! -d "$vundlePath" ]; then
    git clone https://github.com/VundleVim/Vundle.vim.git "$vundlePath";
fi
