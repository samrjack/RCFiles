# Setting up the path variable for all shell environments.
# Path folders are searched in the reverse order that they are listed in this file.

# program files
export PATH="/usr/local/bin:${PATH}"
export PATH="/usr/local/sbin:${PATH}"
export PATH="/sbin:${PATH}"

# Doom emacs
export PATH="${HOME}/.emacs.d/bin:${PATH}"

# Android development
export PATH="/usr/local/android-studio/bin:${PATH}"
export PATH="${HOME}/Android/Sdk/tools:${PATH}"
export PATH="${HOME}/Android/Sdk/tools/bin:${PATH}"
export PATH="${HOME}/Android/Sdk/platform-tools:${PATH}"
export PATH="${HOME}/.local/bin:${PATH}"


# Go
export PATH="${HOME}/go/bin:${PATH}"

# NPM
export PATH="${HOME}/.npm-global/bin:${PATH}"

# Rust
export PATH="${HOME}/.cargo/env:${PATH}"
export PATH="${HOME}/.cargo/bin:${PATH}"

# Haskell
export PATH="${HOME}/.cabal/bin:${PATH}"

# Brew package manager for macos
export PATH="/opt/homebrew/bin:${PATH}"
export PATH="/opt/homebrew/opt/openjdk/bin:${PATH}"
export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:${PATH}"

# Brew package manager for linux
export PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin${PATH+:$PATH}";

# Source a GUIX environment if present
export GUIX_PROFILE="${HOME}/.guix-profile"
export GUIX_PROFILE_SCRIPT="${GUIX_PROFILE}/etc/profile"
if [ -e ${GUIX_PROFILE_SCRIPT} ]; then
   . ${GUIX_PROFILE_SCRIPT}

   # If guix is updated, it needs to source its new location too. However,
   # if it's not updated, then we should overwrite the above variables.
   NEW_GUIX_PROFILE="${HOME}/.config/guix/current"
   NEW_GUIX_PROFILE_SCRIPT="${NEW_GUIX_PROFILE}/etc/profile"
   if [ -e ${NEW_GUIX_PROFILE_SCRIPT} ]; then
       export GUIX_PROFILE=${NEW_GUIX_PROFILE}
       . ${NEW_GUIX_PROFILE_SCRIPT}
   fi
fi
