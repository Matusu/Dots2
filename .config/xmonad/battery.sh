#!/bin/bash

# Get battery information
battery_info=$(acpi -b)
battery_level=$(echo "$battery_info" | grep -oP '\d+%' | tr -d '%')
charging_status=$(echo "$battery_info" | grep -oP 'Charging|Discharging')

# Display different emojis based on the battery level and charging status
if [ "$charging_status" = "Charging" ]; then
        echo " : $battery_level%"
else
    if [ "$battery_level" -le 20 ]; then
        echo " : $battery_level%"
    elif [ "$battery_level" -le 50 ]; then
        echo " : $battery_level%"
    elif [ "$battery_level" -le 80 ]; then
        echo " : $battery_level%"
    else
        echo " : $battery_level%"
    fi
fi
