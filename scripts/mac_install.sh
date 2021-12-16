#!/bin/bash

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew tap d12frosted/emacs-plus
brew install tmux alacritty wget sl ranger pass google-chrome firefox qutebrowser cmatrix pandoc asciidoc texlive zoom htop tldr jq tty-clock bash-completion zsh zsh-autosuggestions zsh-navigation-tools zsh-lovers zsh-syntax-highlighting zsh-completions zsh-you-should-use vlc iterm2 1clipboard background-music emacs-plus@28 --with-imagemagick --with-native-comp --with-dbus --with-modern-alecive-flatwoken-icon

ln -s /usr/local/opt/emacs-plus@28/Emacs.app /Applications

sudo xattr -rd com.apple.quarantine /Applications/qutebrowser.app
ln -s ~/.config/qutebrowser/config.py ~/.qutebrowser/config.py

sudo chmod -R 755 /usr/local/share
