#!/bin/bash

if ! [ -x "$(command -v pass)" ]; then
    echo 'Pass is not installed, please install it before adding extensions.'
    exit 1
fi
if ! [ -x "$(command -v go)" ]; then
    echo 'Golang compiler not installed, please install it before building browswerpass-native.' >&2
    exit 1
fi

# import the GPG key for working with the files.
curl https://maximbaz.com/pgp_keys.asc | gpg --import || \
    curl https://keybase.io/maximbaz/pgp_keys.asc | gpg --import || \
    gpg --recv-keys EB4F9E5A60D32232BB52150C12C87A28FEAC6B20;

git clone https://github.com/browserpass/browserpass-native
cd browserpass-native
make all
make configure
sudo chown ${USER}:${USER} .config/chromium
sudo make install
cd ..
rm -rf browserpass-native
cd /usr/lib/browserpass/
make make hosts-chromium-user
make hosts-firefox-user
make hosts-chrome-user
make hosts-brave-user
make hosts-iridium-user
make hosts-vivaldi-user
make hosts-slimjet-user
make policies-chromium-user
make policies-chrome-user
make policies-brave-user
make policies-iridium-user	
make policies-slimjet-user
make policies-vivaldi-user
