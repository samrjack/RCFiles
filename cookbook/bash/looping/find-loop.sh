#!/bin/bash

## Reference: https://www.man7.org/linux/man-pages/man1/find.1.html
## Find is a very useful tool in bash. It is used to find large numbers of files
## that match given patterns. However, there's lots of nuances to it's use that
## I hope to cover in this file.

### Looping over results
## for has a few ways to loop over results. The one I've found most useful recently
## is a while loop. The reason I've found it useful is I can treat the looping
## construct like a for loop but with safer parsing.
##
## NOTE: don't use for loops with find, for loops break the inputs apart on spaces
## which files often have. This can lead to bad results. The following is much safer

find "." -type f -print0 | while IFS= read -r -d '' file; do
	echo "safely referring to full name of $file"
done

## Another common use case is to use find's built in exec function. This is a bit
## more complex but also more concise for quick processing. This can't be used
## with built in functions, but is good enough for printing or starting a pipe of
## data.

find "." -type f -exec echo "{}" \; # Echos each found file name on separate lines

## Another common variant is when all file names can be used at the same time. In
## that case, end the command with a `\+` instead.

find "." -type f -exec echo "{}" \; # Echos all found file names on one line
