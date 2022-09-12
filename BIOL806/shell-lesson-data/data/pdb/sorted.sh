# Sort files by their length.
# Usage: bash sorted.sh one_or_more_filenames
# wc -l lists the number of lines in the file
# wc means word count, -l means count lines
# $@ means all of the command-line arguments to the shell script
# Bc we don't know number of files. put in double quotes
wc -l "$@" | sort -n
