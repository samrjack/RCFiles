#!/bin/bash

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

if [[ "$#" -lt 1 ]]; then
	find . -type f -not -path '*/.*' -print0 | while IFS= read -r -d '' file; do
		processFile "$file"
	done
else
	processFile "$1"
fi
