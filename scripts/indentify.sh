#!/bin/bash

if [ $# -ne 1 ]; then
	echo $0: Usage: indentify old_indent new_indent path/to/files
	exit 1
fi

#old=$1
#new=$2
files=$1

if [[ -d $files ]]; then
	echo "Indentify-ing the Directory..."
elif [[ -f $files ]]; then
	echo "Indentify-ing the File..."
else
	echo "$files is not valid"
	exit 1
fi

for f in $files; do
	perl -ne '$_ =~ s|^((  )+)|"    " x (length($1)/4)|eg; print $_' < $f
done
