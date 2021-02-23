#!/bin/bash

# Install all the control files and run all install scripts.
#
# Order of behavior:
# - Make symbolic links to all files that are missing.
# - If symbolic link is present but broken, replace it.
# - Prompt for all files that would be replaced:
#   - Keep old config.
#   - Link new config.
#   - See diff and re-prompt.
#   - Do git style merge.

# Make "*" also match hidden files
shopt -s dotglob

#### Functions ####
contains() {
    return 0
}

# Get the path to the config directory for use later
invocationPath=$0
if [ ! -e "$invocationPath" ]; then
	case $invocationPath in
		(*/*) (echo "Issue in case 1"; exit 1);;
		(*) invocationPath=$(command -v -- "$invocationPath") || (echo "Issue in case 2"; exit);;
	esac
fi
configDir=$( cd -P -- "$(dirname -- "$invocationPath")" && pwd -P) || exit


#### Setup directory structure ####
allDirs=$(find $configDir -mindepth 1 -not -path '*/\.git/*' -not -name '.git' -not -path '*/\.svn/*' -not -name '.svn' -type d -print)

for directory in $allDirs; do 
    dirRelativePath=${directory#"$configDir"/}
    homePath="$HOME"/"$dirRelativePath"
    if [ ! -d  $homePath ]; then
        echo "Making directory $homePath"
        mkdir $homePath
    fi
done

#### setup up files ####
allFiles=$(find "$configDir" -not -path '*/\.git/*' -not -path '*/\.svn/*' -not -type d -print)

# A list of files that should not be installed on the system and should instead 
# stay in this folder. Usually are helper files such as install scripts.
unwantedFiles=$( find "$configDir" -maxdepth 1 -name "*.md" -o -name "*.sh" -print )

for file in $allFiles; do 
    # If the file is unwanted, then skip it.
    for unwantedFile in $unwantedFiles; do
        [ "$unwantedFile" = "$file" ] && continue 2
    done

    #Process the file and either insert it or interactively prompt for behavior
    fileRelativePath=${file#"$configDir"/}
    homePath="$HOME"/"$fileRelativePath"
    if [ -L "$homePath" ]; then
        if [ "$(readlink -- "$homePath")" -ef "$file" ]; then
            echo "Config link already exists for $file"
        elif [ ! -e "$homePath" ]; then
            echo "Found broken link. Replacing with $file"
            rm -f "$homePath"
            ln -s "$file" "$homePath"
        else
            echo "link points to something else. $file"
            # TODO user input needed
            # - Replace sim link
            # - other options from below
        fi
    elif [ ! -e "$homePath" ]; then
        echo "making link to $file at $homePath"
        ln -s "$file" "$homePath"
    elif [ -f "$homePath" ]; then
        echo "The file already exists $homePath"
        # TODO user input needed.
        # - replace file
        # - keep file
        # - see diff and return to menu
        # - do a diff merge
    else
        echo "I don't know what this file is... $file"
    fi
done
