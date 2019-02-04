#! /bin/bash
# Install all the control files and associated programs.


### Variables ###

# Names of every file in folder.
filesInFolder=(*);

# Names of files to be ignored during copying
removeFiles=(README.md install.sh git_install.sh update.sh);

# File for installing programs, plugins, etc.
externalInstall=git_install.sh;

### Setup variables###

# Remove the extra files from the list of control files.
for file in ${removeFiles[*]}; do
    filesInFolder=("${filesInFolder[@]/$file}");
done;

# Run any other installs that are needed for the RC files such as plugins
if [-f $externalInstall]; then
    chmod +x $externalInstall;
    ./$externalInstall;
fi

### Copy files ###

for file in ${filesInFolder[*]}; do
    toLocation="$HOME/.$file";
    cp ./$file $toLocation;
    echo $toLocation;
done

