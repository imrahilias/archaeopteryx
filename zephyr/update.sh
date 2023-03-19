#!/bin/bash
## backup stray config files

mydir="/home/m/zephyr/zephyr/$(hostname)"
echo $mydir

## rsnapshot timers
sudo cp /etc/systemd/system/rsnapshot-weekly.timer $mydir/rsnapshot
sudo cp /etc/systemd/system/rsnapshot-daily.timer $mydir/rsnapshot
sudo cp /etc/systemd/system/rsnapshot-hourly.timer $mydir/rsnapshot
sudo cp /etc/systemd/system/rsnapshot-monthly.timer $mydir/rsnapshot
sudo cp /etc/systemd/system/rsnapshot@.service $mydir/rsnapshot

## others
sudo cp /etc/mkinitcpio.conf $mydir/
sudo cp /etc/pacman.d/hooks/nvidia.hook $mydir/
sudo cp /etc/pacman.conf $mydir/
sudo cp /etc/powerpill/powerpill.json $mydir/
sudo cp /etc/rsnapshot.conf $mydir/
sudo cp /etc/fstab $mydir/
sudo cp /etc/X11/xorg.conf $mydir/
sudo cp /etc/profile $mydir/
