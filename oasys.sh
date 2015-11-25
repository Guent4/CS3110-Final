#!/bin/bash
# My first script

types=""

while [ $# -gt 0 ]
do
  temp=''$1
  if [ ${temp:0:1} = "-" ]
    then
      types=$types' %'$1
  else
      types=$types' '$1
  fi
  shift
done

cs3110 run main.ml $types
