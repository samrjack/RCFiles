#!/bin/bash

## Source: https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash
## This uses standard basic getopts as can be
## expected to be apart of every shell.
##
## Downside is that without enhancements, getopts can't handle
## extended parameters like --help

function show_help {
	echo "this is the help"
}

# A POSIX variable
OPTIND=1 # Reset in case getopts has been used previously in the shell.

# Initialize our own variables that will go on to get set below
output_file=""
verbose=0

# This will loop through all the inputs and save the current input's value
# to opt. It will search for the flags listed in the
# first string. See documation for specifcs of syntax.
while getopts "h?vf:" opt; do
	case "$opt" in
	h | \?)
		show_hel
		exit 0
		;;
	v)
		verbose=1
		;;
	f)
		output_file=$OPTARG
		;;
	esac
done

shift $((OPTIND - 1))

[ "${1:-}" = "--" ] && shift

echo "verbose=$verbose, output_file='$output_file', Leftovers: $@"
