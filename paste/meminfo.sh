#!/bin/bash

ADB="adb"
start=`date +%s`
pid=`$ADB shell ps | awk '/com.henry4j.android.experimental/ {print $2}'`
printf "PID:%d\n" "$pid"
echo "time  native   dalvik    other"

function doit()  {
    dt=`date +%s`
    (( diff = dt - start ))
    out=`$ADB shell dumpsys meminfo "$pid" | awk '/allocated/ {printf("%6d %8d %8d", $2, $3, $4)}'`
    printf "%05d %s\n" "$diff" "$out"
}

while :
do
    doit
    sleep 1
done
