#!/bin/bash
cd projects
ls | while read line; do
	cd $line;
	../../gradle/bin/gradle eclipse;
	cd ..;
done
