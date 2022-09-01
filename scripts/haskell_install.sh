#!/bin/bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh || exit 2

echo 'export PATH="${PATH}:${HOME}/.cabal/bin:${HOME}/.ghcup/bin"' >> ~/.shell_setup/path_setup.local.sh

