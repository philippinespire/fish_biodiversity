#!/bin/bash

#Script to update github with message
#Example: bash runGIT.bash "Your Message"

message=$1

git pull
git add --all
git commit -m "$message"
git push

