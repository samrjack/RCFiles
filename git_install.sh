#! /bin/bash
# Installs git repos as necessary for standard setup.

### Vundle install ###

vundleDirectory="$HOME/.vim/bundle";
if [ ! -d $vundleDirectory]; then
    mkdir $vundleDirectory;
fi

git clone https://github.com/VundleVim/Vundle.vim.git "$vundleDirectory/Vundle.vim"
