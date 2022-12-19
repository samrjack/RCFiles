[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# RCFiles

The dotfiles of [samrjack](https://github.com/samrjack "link to github"). I named this repo RCFiles though because at the time the majority of files I used were RC files and now the number of config files has grown. I've worked on these for several years and they have some great functionality in them that I hope is useful. When looking through or working with particular files, first check to see if there is a tangle file in the [tangleDocs](https://github.com/samrjack/RCFiles/tree/main/tangleDocs "link to tangleDocs") folder. The org files in there break up the files into more consumable chunks. If a given file is not yet in there, then search though the config files to view what is available.

## Instructions for usage

To install all the dotfiles, first clone this repository to somewhere permanent (I usually just clone it to the user's home folder) and then run the [configs_install.sh](https://github.com/samrjack/RCFiles/blob/main/scripts/configs_install.sh) script under the scrips folder which proceeds to make symbolic links to all the config files. If a file already exists, the script notifies you and doesn't overwrite the existing file so that a user may choose how to handle the conflict them self and then simple rerun the script again. The install script will need to be run once in a while when new files are added to the repository or moved around as tends to happen.

### Why symbolic linking?

I symbolically link the files because I prefer having a single source of truth for my files. It prevents changes that may be made on the fly from getting lost (since the main RCFiles repo is managed in git). Even now as I'm moving towards literate programming I like knowing that all the files that control my main programs are easily find-able in one place to make debugging easier.

### Hard linking

To hard link instead of symbolically linking, you'll need to change the install script itself. It's not hard but it's not something that I've ever really needed so I've not bothered building in the functionality.

## File structure

- configs: Contains all the config files. They are all hidden files so be sure to enable hidden files or use ls -l to see them.
- tangleDocs: A collection of literate programming configuration files that I'm slowly growing and documenting when I have time. These are quite easy to read in github and are easier to document with decisions and structure so check with these first.
- scripts: A collection of scripts I've written to help me install my settings and programs on new systems. Some of them have only been used once and may yet provide reproducible results, but they're my attempt at automating system installs and reliability.

### Scripts

Here's a list of current scripts and their uses. May not be entirely up to date but it won't be that off.

- `configs_install.sh`: Installs all configs onto the system.
- `doom-emacs_install.sh`: Installs doom emacs which is my config distribution of choice.
- `oh-my-zsh_install.sh`: Installs oh-my-zsh which makes zsh look much nicer.
- `vundle_install.sh`: Installs vudle which is a package manager for vim plugins.
- `fonts_install.sh`: Installs the nerd fonts for the purpose of having images to use places.
- `guix_install.sh`: Installs the guix package manager.
- `npm_install.sh`: Sets up NPM settings.
- `pass-addons_install.sh`: Installs web browser addons for using the pass password manager.
- `SDKman_install.sh`: Installs the SDKman program which helps install sdks such as Java, Kotlin, etc.
- `macos`: A set of scripts specifically for installing my setup and standard programs on a mac computer.
  - `full_system_install.sh`: Should install everying. For use on a brand new system.
  - `programs_install.sh`: A list of install commands for the programs I regularly install on a new machine.
  - `configure_settings.sh`: A set of system manipulations needed on MacOS to get things to work.
  - `make_aliases.sh`: Creates a starter local aliases file to remove some settings from my standard linux alias file.

## Org files, tangle, and literate programming configuration

Literate programming is an interesting idea that allows you to use normal text documents to create configuration files. By doing this, you are able to write about decisions that were made or useful documentation without it filling up the entire final config file. Why not use comments? Because often the formatting and pleasantness of text document helps to identify the important information and the stuff that's just there in case it may be useful later. It also makes things like folding sections or parts of sections far easier as well as making the ordering of sections are parts of sections more clear. Often in comments it can be hard to tell where in a nested section you're at since comments don't easily show structure, but org files sure do!
