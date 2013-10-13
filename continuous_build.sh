#!/bin/bash

if [[ $# != 2 ]]; then
    echo "Usage: continuous_build <h-code-source> <fanuc-macro-program>"
    exit 1
fi

while true
do
    sleep 1
    if [ -n $1 ]; then
        make SOURCE="$1" PROG="$2"
    else
        make
    fi
done
