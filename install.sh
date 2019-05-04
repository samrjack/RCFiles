#! /bin/bash
# Install all the control files and associated programs.


### Variables ###

# Names of every file in folder.
filesInFolder=(*);

# Names of files to be ignored during copying
removeFiles=(README.md install.sh i3 git_install.sh update.sh .git);

# File for installing programs, plugins, etc.
externalInstall=git_install.sh;

### Setup variables###

# Remove the extra files from the list of control files.
for remove in ${removeFiles[*]}; do
    for i in "${!filesInFolder[@]}"; do
        if [[ ${filesInFolder[i]} = $remove ]]; then
            unset 'filesInFolder[i]'
            break;
        fi
    done
done

# Run any other installs that are needed for the RC files such as plugins
if [ -f $externalInstall ]; then
    chmod +x $externalInstall;
    ./$externalInstall;
fi

### Copy files ###

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

echo $DIR

for file in ${filesInFolder[*]}; do
    toLocation="$HOME/.$file";
    baseLocation="$DIR/$file";
    ln -sFi $baseLocation $toLocation;
    echo $toLocation;
done

