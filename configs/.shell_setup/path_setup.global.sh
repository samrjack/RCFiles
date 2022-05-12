# Setting up the path variable for all shell environments.

# program files
export PATH="/usr/local/bin:${PATH}"
export PATH="/sbin:${PATH}"

# Doom emacs
export PATH="${HOME}/.emacs.d/bin:${PATH}"

# Android development
export PATH="/usr/local/android-studio/bin:${PATH}"
export PATH="${HOME}/Android/Sdk/tools:${PATH}"
export PATH="${HOME}/Android/Sdk/tools/bin:${PATH}"
export PATH="${HOME}/Android/Sdk/platform-tools:${PATH}"
export PATH="${HOME}/.local/bin:${PATH}"

# Rust
export PATH="${HOME}/.cargo/env:${PATH}"

# Go
export PATH="/usr/local/go/bin:${PATH}"

# NPM
export PATH="${HOME}/.npm-global/bin:${PATH}"

# Rust build system
export PATH="${HOME}/.cargo/bin:${PATH}"

# Haskell
export PATH="${HOME}/.cabal/bin:${PATH}"

# Source a GUIX environment if present
GUIX_PROFILE="${HOME}/.guix-profile"
GUIX_PROFILE_SCRIPT="$GUIX_PROFILE/etc/profile"
if [ -e "$GUIX_PROFILE_SCRIPT" ]; then
   . "$GUIX_PROFILE_SCRIPT"

   # If guix is updated, it needs to source its new location too. However,
   # if it's not updated, then we should overwrite the above variables.
   NEW_GUIX_PROFILE="${HOME}/.config/guix/current"
   NEW_GUIX_PROFILE_SCRIPT="$NEW_GUIX_PROFILE/etc/profile"
   if [ -e "$NEW_GUIX_PROFILE_SCRIPT" ]; then
       GUIX_PROFILE="$NEW_GUIX_PROFILE"
       . "$NEW_GUIX_PROFILE_SCRIPT"
   fi
fi
