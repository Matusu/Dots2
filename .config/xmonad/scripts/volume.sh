#!/bin/bash
volume=$(amixer get Master | grep -oP '\d+%' | head -1 | tr -d '%')
mute=$(amixer get Master | grep -oP '\[(on|off)\]' | head -1 | tr -d '[]')

if [ "$mute" = "off" ]; then
    echo " : 0%"
else
    # Display different emojis based on the volume level
    if [ "$volume" -eq 0 ]; then
        echo " : $volume%"
    elif [ "$volume" -le 30 ]; then
        echo " : $volume%"
    elif [ "$volume" -le 70 ]; then
        echo " : $volume%"
    else
        echo " : $volume%"
    fi
fi
