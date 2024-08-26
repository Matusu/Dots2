#!/bin/bash
brightness=$(lux -G | tr -d '%')

dunstify -a "Changebrightness" -u low -r 8802 -h int:value:"$brightness" -i $HOME/.config/xmonad/resources/brightness.png "brightness: ${brightness}%" -t 1000
