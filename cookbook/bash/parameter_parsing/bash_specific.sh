#!/bin/bash

## Source: https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash
## This showcases a solution that is bash specific and solves some
## of the problems with getopts without requiring enhanced getopts.

# Save arguments that are not attached to a flag into a list.
# Used for things like a file list.
POSITIONAL_ARGS=()

while [[ $# -gt 0 ]]; do
	case $1 in
	# Assumes parameters are in the form `--extension value`
	-e | --extension)
		EXTENSION="$2"
		shift # past argument
		shift # past value
		;;
	-s=* | --searchpath=*)
		SEARCHPATH="${1#*=}" # Removes tag and `=`. Check doc for info https://tldp.org/LDP/abs/html/string-manipulation.html
		shift                # past argument=value
		;;
	-s | --searchpath)
		SEARCHPATH="$2"
		shift # past argument
		shift # past value
		;;
	# Assumes this is a boolean flag
	--default)
		DEFAULT=YES
		shift # past argument
		;;
	-* | --*)
		echo "Unknown option $1"
		exit 1
		;;
	*) # Other parameters without flags like files

		POSITIONAL_ARGS+=("$1") # save positional arg
		shift                   # past argument
		;;
	esac
done

set -- "${POSITIONAL_ARGS[@]}" # restore positional parameters so they can be read in from inputs as desired

## Instead of the above while loop, I think a for loop could also be used
# for i in "$@"; do
# 	case $i in
# 	-e=* | --extension=*)
# 		EXTENSION="${i#*=}"
#		# shift # past argument=value; only need in while loop
# 		;;
# 	# ... etc
# 	*) # Other parameters without flags like files
#		POSITIONAL_ARGS+=("$1") # save positional arg
# 		;;
# 	esac
# done
## The shifting is still done so that we can reuse

echo "FILE EXTENSION  = ${EXTENSION}"
echo "SEARCH PATH     = ${SEARCHPATH}"
echo "DEFAULT         = ${DEFAULT}"
echo "Number files in SEARCH PATH with EXTENSION:" $(ls -1 "${SEARCHPATH}"/*."${EXTENSION}" | wc -l)

if [[ -n $1 ]]; then
	echo "Last line of file specified as non-opt/last argument:"
	tail -1 "$1"
fi
