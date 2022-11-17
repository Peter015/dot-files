#!/bin/bash

### Variables ###


# Kernel Version
linux_version=$(uname -r | cut -d '-' -f1)
ip_addr=$(ip route get 1.2.3.4 | awk '{print $7}')

while true
do
  # Network
  network=$(ip route get 1.1.1.1 | grep -Po '(?<=dev\s)\w+' | cut -f1 -d ' ')
  interface_easyname=$(dmesg | grep $network | grep renamed | awk 'NF>1{print $NF}')
  ping=$(ping -c 1 www.google.es | tail -1| awk '{print $4}' | cut -d '/' -f 2 | cut -d '.' -f 1)

  # Date
  date=$(date +'%Y-%m-%d')
  time=$(date +'%I:%M:%S %p')

  # Load Average
  loadavg_5min=$(cat /proc/loadavg | awk -F ' ' '{print $2}')
  
  echo "$linux_version | $interface_easyname ($ping ms) | $ip_addr | $loadavg_5min | $time | $date"
  sleep 1
done

