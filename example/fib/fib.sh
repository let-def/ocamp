#!/bin/sh
ARG="$1"
echo "fib $ARG" >&2
if [ "$ARG" -le 1 ]; then
  echo "$ARG"
else
  A=`$ocamp pull ./fib.sh $((ARG-1))`
  B=`$ocamp pull ./fib.sh $((ARG-2))`
  echo $((A+B))
fi 
