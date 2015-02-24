#!/bin/bash
# usage: invoke-sudo.sh [-v] COMMAND
# author: jianingy.yang@gmail.com

euser=root
case "$1" in
  -v) shift; verbose=1 ;;
  -u) shift; euser=$1; shift ;;
esac

if [ "`id -u`" != "`id -u $euser`" ]; then 
  echo "`whoami`: you need $euser priviledge to run this application. Invkoing sudo ..." 
  [ -n "$verbose" ] && echo "[sudo] $@"
  exec sudo -u $euser $@
else
  exec $@
fi
