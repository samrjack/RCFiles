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
sudo make configure
sudo make install
cd ..
rm -rf browserpass-native
