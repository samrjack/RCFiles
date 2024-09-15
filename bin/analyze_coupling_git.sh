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

echo "count,file1,file1TotalCommits,file1CommitPecentage,file2,file2TotalCommits,file2CommitPercentage"

eval "git log ${git_options} --format=format:"" --name-only -- ${positional_args[*]@Q}" |
	awk '
	BEGIN {
		filenum = 0
	}

	/../ {
		commitFiles[filenum++] = $0
	}

	/^$/ {
		for (i = 0; i < filenum; i++) {
			allFiles[commitFiles[i]]++
		}
		for (i = 0; i < filenum - 1; i++) {
			for (j = i+1; j < filenum; j++) {
				pairedFiles[commitFiles[i],commitFiles[j]]++
			}
		}
		filenum = 0
		# split("",commitFiles);
	}

	END {
		for (combined in pairedFiles) {
			count = pairedFiles[combined]
			split(combined, separate, SUBSEP)
			file1 = separate[1]
			file2 = separate[2]
			file1TotalCount = allFiles[file1]
			file2TotalCount = allFiles[file2]
			file1Percentage = 100 * (count / file1TotalCount)
			file2Percentage = 100 * (count / file2TotalCount)
			printf("%d,%s,%d,%5.2f%%,%s,%d,%5.2f%%\n",count,file1,file1TotalCount,file1Percentage,file2,file2TotalCount,file2Percentage)
		}
	}' | sort -nr
