#!/bin/sh

for v in 1 2 3 4 5 6 7 8 9
do
    v_pids=`fuser /dev/tty$v 2>/dev/null`
    for v_pid in $v_pids
    do
	if (ps -p "$v_pid" -o comm= | grep -q '^X')
	then
	    echo -n $v
	    exit 0
	fi
    done
done
exit 1
