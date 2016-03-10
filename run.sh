#!/bin/bash

PORT="80"
CPUS="0"
RES="static"

for i in "$@"
do
case $i in
    -p=*)
    PORT="${i#*=}"
    ;;
    -c=*)
    CPUS="${i#*=}"
    ;;
    -r=*)
    RES="${i#*=}"
    ;;
esac
done

echo "Starting server"
erl <<< "httpd:shell_start([\"${PORT}\",\"${CPUS}\",\"${RES}\"])." > not_log.txt
echo "Server terminated"