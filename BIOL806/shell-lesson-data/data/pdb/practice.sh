# Select lines from the middle of the file
# Usage: bash practice.sh filename end_line num_lines
head -n "$2" "$1" | tail -n "$3"
