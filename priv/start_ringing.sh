#!/bin/sh
gst-launch filesrc location=/media/card/arch_carrier.ogg ! tremor ! alsasink &
echo $! > /tmp/ringtone-player-pid
