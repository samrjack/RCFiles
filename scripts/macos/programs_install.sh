#!/bin/bash

if ! command -v brew &> /dev/null
then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    export PATH="/opt/homebrew/bin:${PATH}"
fi

brew tap buo/cask-upgrade

brew tap buo/cask-upgrade
brew install mas
sudo softwareupdate --install-rosetta

brew install python

pip3 install poetry

brew install npm

mas install 497799835 # Xcode
sudo xcodebuild -license accept

brew install --cask iterm2

brew install --cask alacritty

brew install --cask zoom

brew install --cask vlc

# Emacs build dependencies
brew install gcc
brew install libgccjit
brew install imagemagick
brew install mailutils
brew install dbus
brew install tree-sitter

brew tap d12frosted/emacs-plus
brew update
brew install emacs-plus@28 --with-imagemagick --with-native-comp --with-dbus --with-modern-cg433n-icon --with-mailutils --with-ctags --with-xwidgets

brew install sublime-text

brew install brackets

brew install atom

brew install visual-studio-code

brew install --cask intellij-idea-ce

brew install --cask goland

brew install --cask webstorm

brew install --cask google-chrome

brew install --cask firefox

brew install --cask brave-browser

brew install --cask qutebrowser

brew install --cask slack

brew install --cask gimp

brew install --cask pgadmin4

brew install --cask postman

brew install --cask spotify

brew install --cask karabiner-elements

brew install --cask flameshot

brew install --cask anki

brew install --cask musescore

brew intsall locust

brew install homebrew/cask-versions/virtualbox-beta

brew tap iina/homebrew-mpv-iina
brew install mpv-iina

brew install bash
brew install bash-completion
brew install bash-language-server

brew install zsh
brew install zsh-autosuggestions
brew install zsh-navigation-tools
brew install zsh-lovers
brew install zsh-syntax-highlighting
brew install zsh-completions
brew install zsh-you-should-use

brew install tmux

brew install reattach-to-user-namespace

brew install coreutils

brew install moreutils

brew unlink awk
brew install gawk

brew install mawk

brew install bingrep

brew install grep

brew install ripgrep
brew install ripgrep-all

brew install the_silver_searcher

brew install awscli

brew install aws-iam-authenticator

brew install wget

brew install httpie

brew install fd

brew install jq

brew install yq

brew install cmake

brew install git-annex

brew install jesseduffield/lazygit/lazygit

brew install xdg-ninja

brew install z

brew install sl

brew install ranger

brew install cmatrix

brew install htop

brew install tldr

brew install ledger

brew install tty-clock

brew install nmap

brew install k9s

brew install rabbitmq

brew install pass

brew install pgcli

brew install hstr

brew install py3cairo ffmpeg pango scipy
pip3 install manim

pip3 install git-sim

brew install difftastic

brew install speedtest_cli

brew install youtube-dl

brew install onefetch

brew install neofetch

brew install cloc

brew install pdftk-java

brew tap homebrew-ffmpeg/ffmpeg
brew upfdate

brew install chromaprint

brew tap amiaopensource/amiaos
brew install amiaopensource/amiaos/decklinksdk

brew tap lescanauxdiscrets/tap
brew install lescanauxdiscrets/tap/zvbi

brew uninstall ffmpeg # Uninstall in case it was previously installed
brew install homebrew-ffmpeg/ffmpeg/ffmpeg $(brew options homebrew-ffmpeg/ffmpeg/ffmpeg --compact)

brew install pandoc

brew install asciidoc

brew install sops

brew install postgresql
brew install golang-migrate
brew install sql-language-server

brew install --cask docker

brew install --cask lastpass
brew install lastpass-cli

brew install --cask 1clipboard

brew install --cask background-music

brew install --cask hiddenbar

brew install --cask keycastr

brew install --cask monitorcontrol

mas install 937984704 # Amphetamine

brew install caffeine

brew install --cask dropbox

brew install --cask alfred

brew install --cask go2shell

brew install --cask spectacle

brew install --cask tunnelblick

brew install --cask --cask aws-vpn-client

brew install gnuplot

brew install graphviz

brew install kubernetes-cli

brew install minikube

brew install hyperkit

brew install helm
helm plugin install https://github.com/jkroepke/helm-secrets --version v3.11.0
helm plugin install https://github.com/rimusz/helm-tiller

brew install eksctl

brew install skaffold

brew install kustomize

pip3 install --user localstack
pip3 install --user aws-local

brew install aspell

brew install android-sdk
brew install android-platform-tools

brew install lsusb

brew install editorconfig

brew install direnv

brew install --cask android-studio

brew install go

brew install gopls

brew install golangci-lint

brew install gcc
brew install ccls

brew install java
brew install jdtls

brew install clang-format

brew install kotlin

brew install kotlin-language-server

brew install ktlint

brew install scala
brew install metals # Scala LSP
brew install coursier # Scala artifact fetcher

brew install clojure

brew install clojure-lsp

brew install --cask zprint

brew install clojurescript

brew install python

brew install black

brew install pyenv

pip3 install pyflakes
pip3 install isort

pip3 install pipenv

pip3 install nose3

pip3 install pyright

brew install ghc
brew install haskell-language-server
brew install cabal-install
brew install haskell-stack

brew install elm
brew install elm-format

brew install sbcl

brew install --cask racket

brew install gambit-scheme
brew install sagittarius-scheme

brew install rust
brew install rustc-completion

brew install rustup-init

brew install rust-analyzer

brew install erlang
brew install wrangler

brew install elixir
brew install elixir-ls

brew install ocaml

brew install opam # Ocaml package manager
[[ -d "${HOME}/.opam" ]] || opam init
opam install -y dune # Ocaml build system
opam install -y utop # Ocaml repl
opam install -y ocp-indent # Ocaml auto indent
opam install -y merlin # Ocaml completion engine
opam install -y ocaml-lsp-server # Ocaml lsp server
opam install -y ocamlformat # Code auto-formatter

brew install tidy-html5

brew install texlive

brew install shfmt
brew install shellcheck

brew install grip

brew install prettier

brew install marksman
brew install ltex-ls

brew install terraform
brew install terraform-docs

brew install terraform-ls

brew install terraformer

brew install ansible
brew install ansible-language-server

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash

brew install deno

npm -g install js-beautify
npm -g install stylelint

brew install yaml-language-server

brew tap homebrew/cask-fonts

brew install --cask font-jetbrains-mono

brew install --cask font-3270-nerd-font
brew install --cask font-space-mono-nerd-font
brew install --cask font-open-dyslexic-nerd-font
brew install --cask font-go-mono-nerd-font
brew install --cask font-hasklug-nerd-font
brew install --cask font-symbols-nerd-font
brew install --cask font-symbols-only-nerd-font
brew install --cask font-jetbrains-mono-nerd-font
brew install --cask font-daddy-time-mono-nerd-font
