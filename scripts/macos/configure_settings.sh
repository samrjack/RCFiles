#!/bin/bash

ln -s /usr/local/opt/emacs-plus@28/Emacs.app /Applications

sudo xattr -rd com.apple.quarantine /Applications/qutebrowser.app
ln -s ~/.config/qutebrowser/config.py ~/.qutebrowser/config.py

sudo chmod -R 755 /usr/local/share
