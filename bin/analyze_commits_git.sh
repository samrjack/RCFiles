#!/bin/bash

positional_args=()
git_options=""

while [[ $# -gt 0 ]]; do
	case $1 in
	-g=* | --git-options=*)
		git_options="${1#*=}"
		shift
		;;
	-g | --git-options)
		git_options="$2"
		shift # past argument
		shift # past value
		;;
	-*)
		echo "Unknown option $1"
		exit 1
		;;
	*)
		positional_args+=("$1") # save positional arg
		shift                   # past argument
		;;
	esac
done

echo "word,count"

eval "git log ${git_options} --format=format:%B -- ${positional_args[*]@Q}" |
	# Turn all spaces into newlines so words can be easily disected
	sed 's;[[:space:]]\{1,\};\n;g' |
	# Remove all surrounding punctuation and empty lines
	sed -n 's;[^[:alnum:]]*\([[:alnum:]]\(.*[[:alnum:]]\)*\)[^[:alnum:]]*;\1;p' |
	# Sort all the words
	sort |
	# Combine like-words and count occurances
	uniq -c |
	# Re-sort the now counted words
	sort -r |
	# Turn into a CSV with the word first and count second
	awk '{ print($2","$1) }'
