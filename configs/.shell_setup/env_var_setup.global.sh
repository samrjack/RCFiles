# Preferred editor for local and remote sessions
export EDITOR='vim'

# ssh
export SSH_KEY_PATH="${HOME}/.ssh/rsa_id"

# Rust
export RUST_SRC_PATH="~/.cargo/bin/"

# path to man pages
export MANPATH="/usr/local/man:$MANPATH"
export MANPATH="/home/linuxbrew/.linuxbrew/share/man${MANPATH+:$MANPATH}:";
export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:${INFOPATH:-}";

# Configuration variable
export XDG_CONFIG_HOME="${HOME}/.config"

# Homebrew variables
if test -d "/home/linuxbrew"; then
    export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew";
    export HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar";
    export HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew";
elif test -d "/opt/homebrew"; then
    export HOMEBREW_PREFIX="/opt/homebrew";
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
    export HOMEBREW_REPOSITORY="/opt/homebrew";
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Guix settings
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"

guixSSL="$HOME/.guix-profile/etc/ssl/certs${SSL_CERT_DIR:+:}$SSL_CERT_DIR"
guixGitSSL="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt${GIT_SSL_CAINFO:+:}$GIT_SSL_CAINFO"
if [ -f guixSSL ]; then
    export SSL_CERT_DIR=$guixSSL
    export GIT_SSL_CAINFO=$guixGitSSL
fi
