#!/bin/bash
dunstify -u low -r 8801 -i /home/matusu/.config/udiskie/usb.png {event} {device_presentation} -t 2000
if [ $1 == "device_mounted" ]; then
   alacritty --class "usb" --working-directory /run/media/matusu/
fi
if [ $1 == "device_removed" ]; then
   termpid=$(ps -ef | grep alacritty | grep usb | head -1 | awk '{print$2}')
   if [ -z "$termpid" ]; then 
      echo "null"
   else
      kill $termpid
   fi
fi
