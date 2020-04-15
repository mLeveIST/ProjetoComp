#!/bin/bash

(make clean && make)

for f in exs/*.min; do
  ./minor < $f
  if [ $? -ne 0 ]; then
    echo $f "did not pass"
  else
    echo .
  fi
done

for f in exs/moretests/*.min; do
  ./minor < $f
  if [ $? -ne 0 ]; then
    echo $f "did not pass"
  else
    echo .
  fi
done