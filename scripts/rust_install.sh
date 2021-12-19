#!/bin/bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh || exit 2


echo '# Adding rust binaries to path' >> ~/.shell_setup/path_setup.local.sh
echo 'export PATH=${PATH}:~/.cargo/bin' >> ~/.shell_setup/path_setup.local.sh


echo '# Rust location' >> ~/.shell_setup/env_setup.local.sh
echo 'export RUST_SRC_PATH="~/.cargo/bin/"' >> ~/.shell_setup/env_setup.local.sh

