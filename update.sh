#! /bin/bash
# Update the files in this repo with their current equivilants in the home directory.
# This will overwrite the files, however the magic of git will keep the originals safe.

### Variables ###

# Names of every file in folder.
filesInFolder=(*);

# Names of files to be ignored during copying
removeFiles=(README.md install.sh git_install.sh update.sh);

### Setup variables###

# Remove the extra files from the list of control files.
for file in ${removeFiles[*]}; do
    filesInFolder=("${filesInFolder[@]/$file}");
done;

### Copy files ###

for file in ${filesInFolder[*]}; do
    fromLocation="$HOME/.$file";
    if [ -f $fromLocation ]; then
        cp $fromLocation ./$file;
        echo $fromLocation;
    fi
done

