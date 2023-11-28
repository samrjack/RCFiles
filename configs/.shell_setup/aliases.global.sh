#### Aliases
## Simple name aliases
alias clc='clear'

alias :q="exit"
alias logout='exit'

alias open="xdg-open"

alias locst="localstack"

alias gitsum="onefetch"
alias syssum="neofetch"

## Standard library shadowing
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias tmux='tmux -2'

## Grep aliases
alias grep="grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,build,.idea} --exclude={tags}"
alias search="grep --exclude-dir='vendor' --exclude-dir='.git' --exclude-dir='node-modules'"

## ls aliases
alias ls='ls --color=auto --group-directories-first -bF'
alias ll='ls -alh'
alias la='ls -hA'
alias l='ls -CFlh'

## C++ compiler aliases
alias g++='g++ -Wall -g -std=c++0x'
alias g++p='g++ -pthread -std=c++11'
alias gccp='gcc -lpthread -Wall -Werror'
alias gcc='gcc -Wall -Werror'

## vim shortcuts
alias tmuxrc="vim ~/.tmux.conf"
alias vimrc="vim ~/.vimrc"
# An alias to read from stdin without requiring the output to be saved
alias v='vim -c "setlocal buftype=nofile bufhidden=hide noswapfile" -'

## Emacs shortcuts
alias tmacs="emacs -nw"
alias temacs="tmacs"
alias emacsc="emacsclient -c -n"
alias ec="emacsc"
alias temacsc="emacsc -t"
alias tec="ec -t"
alias weather="curl 'wttr.in?m'"

## Git aliases
# From https://stackoverflow.com/questions/1057564/pretty-git-branch-graphs#answer-9074343
alias gl1=" git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all"
alias gl2="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all"
alias gl="gl1"
alias lg="gl"
alias git-root='cd $(git rev-parse --show-toplevel)'
alias git-r='git rev-parse --show-toplevel'
alias gr="git-r"
alias cdgr="git-root"



## Docker aliases
alias dockerc="docker-compose"
alias dcu="docker-compose up"
alias dcub="docker-compose up --build"

## Convenient shortcut behavior
# Add a stream of numbers
alias add="awk '{ sum += \$1 } END { print sum }'"
# Start a python server so other local devices can access files
alias server="echo 'http://localhost:8000/'; python3 -m http.server"
# Standard un-tar command
alias untar="tar -xvf"
# Show all folders in path variable
alias viewPath='echo $PATH | sed -e "s/:/\n/g"'
alias path='echo $PATH | sed -e "s/:/\n/g"'
# Set proper window size
alias xrs='set -o noglob; eval `resize`; unset noglob'
# Restart yabai which is a tiling window manager for mac
alias yabai-restart='launchctl kickstart -k "gui/${UID}/homebrew.mxcl.yabai"'


#### Useful functions

# Open instance of magit in repo
function magit () {
    git_root=$(git rev-parse --show-toplevel)
    # magitExecute="(let ((display-buffer-alist \`((\"^\\*magit: \" display-buffer-same-window) ,display-buffer-alist))) (magit-status \"${git_root}\"))"
    magitExecute="(magit-status)"
    echo "executing: ${magitExecute}"
    if [ $? -eq 0 ]; then
        if ! emacsclient -e 0 >&/dev/null; then
            echo "in new emacs instance"
            emacs -nw -n --eval=${magitExecute}
        else
            echo "in emacsclient"
            emacsclient -nw -e ${magitExecute}
        fi
    fi
}

# Suspend the computer
function hibernate () {
    if [[ $(which systemctl) ]]; then
        systemctl suspend;
    else
        echo "sorry, don't know how yet.";
    fi
}
