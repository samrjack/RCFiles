# Aliases
alias ls='ls --color=auto --group-directories-first -bF'
alias ll='ls -alh'
alias la='ls -hA'
alias l='ls -CFlh'

alias clc='clear'

alias :q="exit"
alias logout='exit'

alias g++='g++ -Wall -g -std=c++0x'
alias g++p='g++ -pthread -std=c++11'
alias gccp='gcc -lpthread -Wall -Werror'
alias gcc='gcc -Wall -Werror'

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias grep="grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,build,.idea} --exclude={tags}"

alias tmux='tmux -2'

alias tmuxrc="vim ~/.tmux.conf"
alias vimrc="vim ~/.vimrc"

alias emacs="emacs -nw"
alias emacsclient="emacsclient -t"

alias open="xdg-open"

alias xrs='set -o noglob; eval `resize`; unset noglob'

# An alias to read from stdin without requiring the output to be saved
alias v='vim -c "setlocal buftype=nofile bufhidden=hide noswapfile" -'

# From https://stackoverflow.com/questions/1057564/pretty-git-branch-graphs#answer-9074343
alias gl1=" git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all"
alias gl2="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all"
alias gl="gl1"
alias lg="gl"

alias yabai-restart='launchctl kickstart -k "gui/${UID}/homebrew.mxcl.yabai"'

