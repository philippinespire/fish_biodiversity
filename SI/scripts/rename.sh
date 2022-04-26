#!/bin/bash

#script for renaming gzipped files
#modify imput file type and sed command as needed


for i in *.gz; do
  mv "$i" "`echo $i | sed "s/.F./.1./"`";
done
