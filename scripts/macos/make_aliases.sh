#!/bin/bash

echo "# When coreutils is installed,"                          >> ~/.shell_setup/aliases.local.sh
echo "# ls does as expected, but when"                         >> ~/.shell_setup/aliases.local.sh
echo "# core utils is not present,"                            >> ~/.shell_setup/aliases.local.sh
echo "# ls uses the MacOS flag of -G"                          >> ~/.shell_setup/aliases.local.sh
echo "# instead of --color."                                   >> ~/.shell_setup/aliases.local.sh
echo "#alias ls='ls -G -bF'"                                   >> ~/.shell_setup/aliases.local.sh
echo "unalias open"                                            >> ~/.shell_setup/aliases.local.sh
echo "alias finder='open'"                                     >> ~/.shell_setup/aliases.local.sh
echo ""                                                        >> ~/.shell_setup/aliases.local.sh
echo "# if type brew &>/dev/null; then"                          >> ~/.shell_setup/aliases.local.sh
echo "#     FPATH=$(brew --prefix)/share/zsh-completions:$FPATH" >> ~/.shell_setup/aliases.local.sh
echo "# "                                                        >> ~/.shell_setup/aliases.local.sh
echo "#     autoload -Uz compinit"                               >> ~/.shell_setup/aliases.local.sh
echo "#     compinit"                                            >> ~/.shell_setup/aliases.local.sh
echo "# fi"                                                      >> ~/.shell_setup/aliases.local.sh
