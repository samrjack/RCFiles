#!/usr/bin/env bash
set -euo pipefail

tmp=$(mktemp)
wget 'https://sv.gnu.org/people/viewgpg.php?user_id=15145' -qO - | sudo -i gpg --import -
wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh -qO "$tmp" && chmod 777 $tmp && sudo "$tmp"
rm $tmp
sudo guix pull
guix pull

# After install, run this to fix the error:
# "setlocale: LC_ALL: cannot change locale"
ROOT_PROFILE="/root/.profile"
sudo grep "GUIX_LOCPATH" "$ROOT_PROFILE" || (sudo echo 'GUIX_LOCPATH=$HOME/.guix-profile/lib/locale' | sudo tee -a "$ROOT_PROFILE")
sudo guix install glibc-locales
