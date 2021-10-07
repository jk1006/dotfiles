#!/bin/bash

for f in $(find ~/personal_workspace/notes -name '*.md'); 
do pandoc -s -o "${f%md}pdf" "${f}";
done
