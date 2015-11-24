#!/bin/bash
echo "compiling..."

while [ "$1" != "" ]; do
  cs3110 compile $1
  shift
done