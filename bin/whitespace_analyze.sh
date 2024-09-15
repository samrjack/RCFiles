#!/bin/bash

positional_args=()
# show_hidden=1

while [[ $# -gt 0 ]]; do
	case $1 in
	-H | --hidden)
		show_hidden=true
		shift
		;;
	-*)
		echo "Unknown option $1"
		exit 1
		;;
	*)
		positional_args+=("$1")
		shift
		;;
	esac
done

function processFile {
	local file="$1"
	tmpFile=$(grep -v "^[[:space:]]*$" "$file")
	numLines=$(echo "$tmpFile" | wc -l)
	startingWhitespace=$(echo "$tmpFile" | sed -e 's/^\([[:space:]]*\).*/\1/')
	tabs=$(echo "$startingWhitespace" | awk '{count += gsub("\t","&")} END {print count}')
	spaces=$(echo "$startingWhitespace" | awk '{count += gsub(" ","&")} END {print count}')
	whitespace=$(echo "$startingWhitespace" | awk '{count += (gsub("[:spaces:]*","&") - 1)} END {print count}')
	whitespaceRatio=$(awk "BEGIN { printf(\"%.1f\", ${whitespace}/${numLines}) }")
	echo "${file},${numLines},${tabs},${spaces},${whitespace},${whitespaceRatio}"
}

echo "FileName,NumberOfLines,NumberOfTabs,NumberOfSpaces,NumberOfWhitespace,AvgWhitespacePerLine"

# If no paths are specified, then just default to the current directory
if [[ ${#positional_args[@]} -eq 0 ]]; then
	positional_args+=(".")
fi

for location in "${positional_args[@]}"; do
	if [[ -f "$location" ]]; then
		processFile "$location"
	elif [[ -d "$location" ]]; then
		if [[ $show_hidden ]]; then find "$location" -type f -print0; else find "$location" -type f -not -path '*/.*' -print0; fi |
			while IFS= read -r -d '' file; do
				processFile "$file"
			done
	elif [[ -e "$location" ]]; then
		>&2 echo "issue detected, $location is not a file or directory"
	else
		>&2 echo "file not found: $location"
	fi
done
