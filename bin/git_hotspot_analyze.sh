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
	-H | --hidden)
		show_hidden=true
		shift
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

function processFile {
	local file=$1
	codeLines=$(cloc --sum-one "$file" | awk '/^SUM/{print $NF}')
	commitCount=$(eval "git rev-list ${git_options} --count HEAD -- \"$file\"")
	authors=$(if [[ -d "$file" ]]; then eval "git shortlog -ns ${git_options} HEAD -- \"$file\""; else eval "git shortlog --follow -ns ${git_options} HEAD -- \"$file\""; fi)
	authorCount=$(echo "$authors" | wc -l)
	mainRatio=$(echo "$authors" | awk 'BEGIN { otherContribs = 0 } NR == 1 { mainContrib = $1 } NR > 1 {otherContribs += $1} END { if (mainContrib > 0) printf("%5.2f%%", (100 * mainContrib / (mainContrib + otherContribs))) }')
	echo "${file},${codeLines},${commitCount},${authorCount},${mainRatio}"
}

echo "filename,codeLines,totalCommits,numAuthors,authorRatio"

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
