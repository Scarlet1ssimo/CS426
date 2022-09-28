#!/bin/bash

if ! diff $fname.ll $fname.ac.ll; then
  >&2 echo "Fail $fname"
  exit 1
else
  echo "Pass $fname"
fi 
