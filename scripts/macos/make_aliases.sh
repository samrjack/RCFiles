#!/bin/bash

echo "alias ls='ls -G -bF'"                                    >> ~/.shell_setup/aliases.local.sh
echo "unalias open"                                            >> ~/.shell_setup/aliases.local.sh
echo ""                                                        >> ~/.shell_setup/aliases.local.sh
echo "if type brew &>/dev/null; then"                          >> ~/.shell_setup/aliases.local.sh
echo "    FPATH=$(brew --prefix)/share/zsh-completions:$FPATH" >> ~/.shell_setup/aliases.local.sh
echo ""                                                        >> ~/.shell_setup/aliases.local.sh
echo "    autoload -Uz compinit"                               >> ~/.shell_setup/aliases.local.sh
echo "    compinit"                                            >> ~/.shell_setup/aliases.local.sh
echo "fi"                                                      >> ~/.shell_setup/aliases.local.sh
