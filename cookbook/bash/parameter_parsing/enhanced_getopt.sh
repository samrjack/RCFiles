#!/bin/bash

## Source: https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash
## This is the most complete way to simply handle passed in parameters.
## This should be able to handle all the following inputs:
#
# myscript -vfd ./foo/bar/someFile -o /fizz/someOtherFile
# myscript -v -f -d -o/fizz/someOtherFile -- ./foo/bar/someFile
# myscript --verbose --force --debug ./foo/bar/someFile -o/fizz/someOtherFile
# myscript --output=/fizz/someOtherFile ./foo/bar/someFile -vfd
# myscript ./foo/bar/someFile -df -v --output /fizz/someOtherFile
#
## The one downside is that this version of getopt isn't installed on every
## system so this is less portable. That being said, it IS installable on most
## systems. For example, on Macs, it requires:
#
# brew install gnu-getopt
#
## But will require some finagling to get it to work properly

# More safety, by turning some bugs into errors.
set -o errexit -o pipefail -o noclobber -o nounset

# Check if a compatible version of getopt is installed
# ignore errexit with `&& true`
getopt --test >/dev/null && true
if [[ $? -ne 4 ]]; then
	echo "I'm sorry, getopt --test failed in this environment."
	exit 1
fi

# Define variables to look for
# option --output/-o requires 1 argument
LONGOPTS=debug,force,output:,verbose
OPTIONS=dfo:v

# -temporarily store output to be able to check for errors
# -activate quoting/enhanced mode (e.g. by writing out “--options”)
# -pass arguments only via   -- "$@"   to separate them correctly
# -if getopt fails, it complains itself to stdout
PARSED=$(getopt --options=$OPTIONS --longoptions=$LONGOPTS --name "$0" -- "$@") || exit 2
# read getopt’s output this way to handle the quoting right:
eval set -- "$PARSED"

d=n f=n v=n outFile=-
# now enjoy the options in order and nicely split until we see --
while true; do
	case "$1" in
	-d | --debug)
		d=y
		shift
		;;
	-f | --force)
		f=y
		shift
		;;
	-v | --verbose)
		v=y
		shift
		;;
	-o | --output)
		outFile="$2"
		shift 2
		;;
	--)
		shift
		break
		;;
	*)
		echo "Programming error"
		exit 3
		;;
	esac
done

# handle non-option arguments
if [[ $# -ne 1 ]]; then
	echo "$0: A single input file is required."
	exit 4
fi

echo "verbose: $v, force: $f, debug: $d, in: $1, out: $outFile"
