#!/bin/sh

# Create the list of code for the Markdown file
# - run from location of code

ls -1 *.for | awk '{printf "||[`%s`](https:/github.com/OpenWaterFoundation/cdss-app-statemod-fortran/tree/master/src/main/fortran/%s)\n", $1, $1}' > junk
