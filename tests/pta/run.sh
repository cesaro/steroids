#!/bin/bash

WILDCARD=*.ll

if [ "$1" != "" ]; then WILDCARD="$1"; fi

set -e
for ll in $WILDCARD;
do
   if [ ! -f $ll ];
   then
      echo "$ll: file not found"
      exit 1
   fi

   echo =================================================================
   echo "File: $ll"
   TESTS=$(grep ' TEST: ' "$ll" | sed "s/.*TEST: //; s/^/pta-dump /; s!%!'$ll'!")
   echo "Found tests:"
   echo "$TESTS"
   echo

   echo "set -xe; $TESTS" | bash
done
