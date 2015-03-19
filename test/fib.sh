#!/bin/sh
ARG="$1"
echo "fib $ARG" >&2
if [ "$ARG" -le 1 ]; then
  echo 1
else
  A=`$OCAMP ./fib.sh $((ARG-1))`
  B=`$OCAMP ./fib.sh $((ARG-2))`
  echo $((A+B))
fi 
