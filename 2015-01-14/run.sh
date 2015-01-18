#!/bin/bash

args=("A M 0800" "A M 1200" "A M 1800" "A M 2200" \
      "P D 0800" "P D 1200" "P D 1800" "P D 2200")

i=0
while [ $i -lt 8 ]
do
    echo "${args[$i]}:"
    ./shortest_path ${args[$i]}
    i=$(($i+1))
done
