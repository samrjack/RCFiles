#! /bin/bash
# Installs git repos as necessary for standard setup.

### Vundle install ###

vundleDirectory="$HOME/.vim/bundle";
if [ ! -d $vundleDirectory ]; then
    mkdir -vp $vundleDirectory;
fi

vundleName="Vundle.vim";
if [ ! -d $vundleName ]; then
    git clone https://github.com/VundleVim/Vundle.vim.git "$vundleDirectory/Vundle.vim";
fi
