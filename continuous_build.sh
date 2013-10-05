#!/bin/bash

while true
do
    sleep 1
    if [ -n $1 ]; then
        make PROG="$1"
    else
        make
    fi
done
