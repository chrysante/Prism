#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJ_DIR="$SCRIPT_DIR/.."

format_dir() {
    $SCRIPT_DIR/_impl/format.sh $PROJ_DIR/$1
}

format_dir include
format_dir src
format_dir test
