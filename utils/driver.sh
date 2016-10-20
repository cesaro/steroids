#!/bin/bash

# variables
FILE=
ARGS=
LIMIT=0
REPLAY=
ROOT=`dirname $0`/../

usage ()
{
   cat >&2 <<XXX
Usage: driver.sh PATH [ARGS] [-limit N] [-replay TID1 N1 TID2 N2...]
XXX
   exit 1
}

parse_opts ()
{
   if test $# -lt 1;
   then
      usage
   fi

   #echo all $*
   #echo number $#

   FILE=$1
   shift

   # parse args and limit
   while test $# -gt 0
   do
      #echo Arg: $1
      case $1 in
      -limit)
         LIMIT="$2"
         shift
         ;;

      -replay)
         shift
         REPLAY="$*"
         break
         ;; 

      *)
         ARGS="$ARGS $1"
         ;;
      esac
      shift
   done
}

main ()
{
   parse_opts "$@"
   echo "file   '$FILE'"
   echo "args   '$ARGS'"
   echo "limit  '$LIMIT'"
   echo "replay '$REPLAY'"

   echo Optimizing "$FILE"...
   opt-3.7 -O3 $FILE > /tmp/program.bc
   if test $? != 0; then exit $?; fi
   llvm-dis-3.7 /tmp/program.bc
   if test $? != 0; then exit $?; fi
   echo 'Done, see /tmp/program.{bc,ll}'

   echo Linking runtime...
	llvm-link-3.7 /tmp/program.bc $ROOT/rt/rt.ll -o /tmp/input.bc
   if test $? != 0; then exit $?; fi
   llvm-dis-3.7 /tmp/input.bc
   if test $? != 0; then exit $?; fi
   echo 'Done, see /tmp/input.{bc,ll}'

   echo 'Yesssssss, now feeling fantaaastic!'
   CMD="$ROOT/tools/stid/main /tmp/input.bc $ARGS -limit $LIMIT -replay $REPLAY"
   echo $CMD
   $CMD
}

main "$@"
