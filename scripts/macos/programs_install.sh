#!/bin/bash

if ! command -v brew &> /dev/null
then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

brew tap buo/cask-upgrade
brew install mas

brew install --cask iterm2

brew install --cask alacritty

brew install --cask zoom

brew install --cask vlc

brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-imagemagick --with-native-comp --with-dbus --with-modern-alecive-flatwoken-icon

brew install sublime-text

brew install brackets

brew install atom

brew install visual-studio-code

brew install --cask intellij-idea-ce

brew install --cask goland

brew install --cask google-chrome

brew install --cask firefox

brew install --cask brave-browser

brew install --cask qutebrowser

brew install --cask slack

brew install --cask gimp

brew install --cask pgadmin4

brew install coreutils

brew install awscli

brew install aws-iam-authenticator

brew install tmux

brew install wget

brew install httpie

brew install sl

brew install fd

brew install ranger

brew install pass

brew install cmatrix

brew install pandoc

brew install asciidoc

brew install htop

brew install tldr

brew install jq

brew install tty-clock

brew install ffmpeg

brew install bash
brew install bash-completion

brew install zsh
brew install zsh-autosuggestions
brew install zsh-navigation-tools
brew install zsh-lovers
brew install zsh-syntax-highlighting
brew install zsh-completions
brew install zsh-you-should-use

brew install cmake

brew install ripgrep

brew install ag

brew install ledger

brew install cloc

brew install sops

brew install k9s

brew install rabbitmq

brew install pgcli

brew install nmap

brew install hstr

brew install z

brew install postgresql
brew install golang-migrate

brew install --cask docker

brew install --cask lastpass
brew install lastpass-cli

brew install --cask 1clipboard

brew install --cask background-music

mas install 937984704 # Amphetamine

brew install caffeine

brew install --cask dropbox

brew install --cask alfred

brew install --cask go2shell

brew install --cask spectacle

brew install --cask tunnelblick

brew install --cask --cask aws-vpn-client

brew install gnuplot

brew install kubernetes-cli

brew install helm
helm plugin install https://github.com/jkroepke/helm-secrets --version v3.11.0
helm plugin install https://github.com/rimusz/helm-tiller

brew install eksctl

brew install minikube

brew install hyperkit

brew install aspell

brew install android-sdk
brew install android-platform-tools

brew install go

brew install gopls

brew install golangci-lint

brew install black

brew install ghc
brew install haskell-language-server

brew install --cask racket

brew install tidy-html5

mas install 497799835 # Xcode

brew install texlive

brew install grip

brew install npm

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash

npm -g install js-beautify
npm -g install stylelint
