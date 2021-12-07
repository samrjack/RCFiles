#!/bin/bash
# SDKman is a tool for installing and managing language SDKs such
# as JAVA, KOTLIN, and SCALA. Info can be found at https://sdkman.io/usage.

# Download the installation script which doesn't modify configs since
# the modifications are already in place.
if [ ! -d "${HOME}/.sdkman" ]; then
    curl -s "https://get.sdkman.io?rcupdate=fase" | bash
    source "$HOME/.sdkman/bin/sdkman-init.sh"
fi
