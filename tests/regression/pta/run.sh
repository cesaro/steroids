#!/bin/bash

set -e
set -x

#pta-dump alloca1.ll
#pta-dump alloca2.ll
#pta-dump alloca3.ll
#pta-dump alloca4.ll

#pta-dump phi1.ll

#pta-dump select1.ll

#pta-dump null1.ll
#pta-dump null2.ll

#pta-dump inval1.ll

#pta-dump fixpoint1.ll
#pta-dump fixpoint2.ll

#pta-dump gep1.ll
#pta-dump gep2.ll
pta-dump gep3.ll

#WILDCARD=*.ll
#
#if [ "$1" != "" ]; then WILDCARD="$1"; fi
#
#set -e
#for ll in $WILDCARD;
#do
#   if [ ! -f $ll ];
#   then
#      echo "$ll: file not found"
#      exit 1
#   fi
#
#   echo =================================================================
#   echo "File: $ll"
#   TESTS=$(grep ' TEST: ' "$ll" | sed "s/.*TEST: //; s/^/pta-dump /; s!%!'$ll'!")
#   echo "Found tests:"
#   echo "$TESTS"
#   echo
#
#   echo "set -xe; $TESTS" | bash
#done
