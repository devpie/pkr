#!/bin/bash
cd projects;
mkdir ${1,,};
cd ${1,,};
mkdir -p src/main/scala/de/thm/pkr
echo "package de.thm.pkr

object Main extends App {

}" > "src/main/scala/de/thm/pkr/$1.scala"
cp ../../build.gradle.template ./build.gradle
gradle eclipse
