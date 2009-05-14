#!/bin/sh

# Do the crude thing until we figure out why ringtone-player-pid ends up empty:
killall gst-launch-0.10
#kill `cat /tmp/ringtone-player-pid`
#rm -f /tmp/ringtone-player-pid
