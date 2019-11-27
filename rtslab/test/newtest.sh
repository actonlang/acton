#! /bin/bash

name="$(echo $1 | sed 's/test_//g')"

if [[ -z $name ]]
then
	echo "Usage: $0 <name>"
	exit 1
fi

name="test_$name"

if [[ -d $name ]]
then
	echo -e "\x1b[31;1mError \x1b[0;1m$name\x1b[m already exists"
	exit 1
fi

echo -e "Creating new test skeleton: \x1b[1m$name\x1b[m"

cp -R test_example $name
cd $name
mv test_example.c ${name}.c

