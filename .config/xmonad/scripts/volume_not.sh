#!/bin/bash
volume=$(amixer get Master | grep -oP '\d+%' | head -1 | tr -d '%')
mute=$(amixer get Master | grep -oP '\[(on|off)\]' | head -1 | tr -d '[]')

function send_notification(){
   dunstify -a "Changevolume" -u low -r 8801 -h int:value:"$volume" -i $1 "volume: ${volume}%" -t 1000
}

if [ "$mute" = "off" ]; then
   dunstify -a "Changevolume" -u low -r 8801 -h int:value:"0" -i $HOME/.config/xmonad/resources/mute_16105278.png "volume: 0%" -t 1000
else
    # Display different emojis based on the volume level
    if [ "$volume" -eq 0 ]; then
      send_notification $HOME/.config/xmonad/resources/mute_16105278.png
    elif [ "$volume" -le 30 ]; then
      send_notification $HOME/.config/xmonad/resources/speaker_16105245.png
    elif [ "$volume" -le 70 ]; then
      send_notification $HOME/.config/xmonad/resources/low-volume_16105245.png
    else
      send_notification $HOME/.config/xmonad/resources/high-volume_16105043.png
    fi
fi


