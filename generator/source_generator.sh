#!/bin/sh

get_output_argument() {
    local output=""
    while [[ $# -gt 0 ]]; do
        case $1 in
            -o|--output)
                output="$2"
                shift 2
                ;;
            *)
                shift 1
                ;;
        esac
    done
    echo "$output"
}

strip_to_src_or_include() {
    local path="$1"
    while [[ ! $(basename "$path") =~ ^(src|include)$ ]]; do
        path=$(dirname "$path")
    done
    echo "$path"
}


GEN_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJ_DIR=$GEN_DIR/..
TOOL_DIR=$PROJ_DIR/tool

source $PROJ_DIR/pyenv/bin/activate

$PROJ_DIR/pyenv/bin/python3 -B $PROJ_DIR/generator/source_generator.py $@

output=$(get_output_argument $@)

output_top_level=$(strip_to_src_or_include $output)

$TOOL_DIR/_impl/include-guard-generator.pl -r $output_top_level -i $output
clang-format -i $output
