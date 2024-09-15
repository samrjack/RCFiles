echo "count,file1,file1TotalCommits,file1CommitPecentage,file2,file2TotalCommits,file2CommitPercentage"
git log --format=format:"" --name-only |
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
