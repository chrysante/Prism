#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJ_DIR="$SCRIPT_DIR/../.."

# Variable that will hold the name of the clang-format command
FMT=""

# Some distros just call it clang-format. Others (e.g. Ubuntu) are insistent
# that the version number be part of the command. We prefer clang-format if
# that's present, otherwise we work backwards from highest version to lowest
# version.
for clangfmt in clang-format{,-{4,3}.{9,8,7,6,5,4,3,2,1,0}}; do
    if which "$clangfmt" &>/dev/null; then
        FMT="$clangfmt"
        break
    fi
done

# Check if we found a working clang-format
if [ -z "$FMT" ]; then
    echo "failed to find clang-format"
    exit 1
fi

function format() {
#   This only formats edited files in version control.
#       for file in $(git status --porcelain | awk 'match($1, "M"){print $2}'); do
#   This formats all files in given directory
    for entry in $1/*; do
        # Recursively search subdirectories
        if [[ -d $entry ]]; then
            format $entry
            continue
        fi
        if [[ $entry != *.h ]] && [[ $entry != *.cc ]] && [[ $entry != *.hpp ]] && [[ $entry != *.cpp ]] && [[ $entry != *.m ]] && [[ $entry != *.mm ]] && [[ $entry != *.metal ]]; then
            continue
        fi
        if [[ $entry == *.metal ]]; then
            # We set the column limit to 120 for metal files because otherwise the shader declaration look horrible
            ${FMT} -i --style=file:$PROJ_DIR/metal.clang-format ${entry} &
            continue
        fi
        ${FMT} -i ${entry} &
    done
}

if [ $# -eq 0 ]; then
    echo "Usage: ./format.sh directory/to/format"
    echo "Warning: Don't use '.' here, as it would take forever to recursively search all subdirs of the project."
    exit 0
fi

# Check all of the arguments first to make sure they're all directories
for dir in "$@"; do
    if [ ! -d "${dir}" ]; then
        echo "${dir} is not a directory";
    else
        format ${dir};
    fi
done
