#!/bin/bash
echo 'export PATH="/opt/homebrew/bin:${PATH}"'
echo 'export PATH="$(brew --prefix)/opt/coreutils/libexec/gnubin:${PATH}"' >> ~/.shell_setup/path_setup.local.sh
