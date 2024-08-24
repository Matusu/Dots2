#!/bin/bash
volume=$(amixer get Master | grep -oP '\d+%' | head -1 | tr -d '%')
mute=$(amixer get Master | grep -oP '\[(on|off)\]' | head -1 | tr -d '[]')

function send_notification(){
   dunstify -a "Changevolume" -u low -r 8801 -h int:value:"$volume" -i $1 "volume: ${volume}%" -t 1000
}

if [ "$mute" = "off" ]; then
   send_notification $HOME/.config/xmonad/resources/volume\(1\).png
else
    # Display different emojis based on the volume level
    if [ "$volume" -eq 0 ]; then
      send_notification $HOME/.config/xmonad/resources/volume\(1\).png
    elif [ "$volume" -le 30 ]; then
      send_notification $HOME/.config/xmonad/resources/volume.png
    elif [ "$volume" -le 70 ]; then
      send_notification $HOME/.config/xmonad/resources/low-volume.png
    else
      send_notification $HOME/.config/xmonad/resources/volume-up.png
    fi
fi


