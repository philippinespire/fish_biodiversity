#!/bin/bash

message=$1

git pull
git add --all
git commit -m "$message"
git push

