#!/usr/bin/env bash
BASEDIR=$(dirname "$0")

function run() {
  cd ${BASEDIR}
  mvn compile
  VERSION=`cat target/classes/version.txt`
  env MAVEN_OPTS="-Xmx6g -Xms2g -Xss4m -Xverify:none -XX:MaxPermSize=1024m -XX:+UseG1GC -XX:+AggressiveOpts" mvn "-Dproject.version=$VERSION" exec:java
}

run
