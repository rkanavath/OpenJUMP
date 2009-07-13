#!/bin/sh

diff -Nra --exclude '.svn' --exclude '*.class' --exclude '*.jar' --exclude '*.zip' --exclude '.DS_Store' h2reference h2spatial > h2spatial.patch
