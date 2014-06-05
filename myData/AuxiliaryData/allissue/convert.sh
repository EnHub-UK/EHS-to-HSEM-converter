#!/bin/bash -

set -o nounset                              # Treat unset variables as an error

for name in "$@"; do
	 unrtf --text $name > $name.txt
   echo $name
	 rm $name
done

