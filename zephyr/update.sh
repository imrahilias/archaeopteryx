#!/bin/bash
## backup stray config files

## rsnapshot timers
sudo cp /etc/systemd/system/rsnapshot-weekly.timer ~/zephyr/rsnapshot
sudo cp /etc/systemd/system/rsnapshot-daily.timer ~/zephyr/rsnapshot
sudo cp /etc/systemd/system/rsnapshot-hourly.timer ~/zephyr/rsnapshot
sudo cp /etc/systemd/system/rsnapshot-monthly.timer ~/zephyr/rsnapshot
sudo cp /etc/systemd/system/rsnapshot@.service ~/zephyr/rsnapshot

## others
sudo cp /etc/mkinitcpio.conf ~/zephyr
sudo cp /etc/pacman.d/hooks/nvidia.hook ~/zephyr
sudo cp /etc/pacman.conf ~/zephyr
sudo cp /etc/powerpill/powerpill.json ~/zephyr
sudo cp /etc/rsnapshot.conf ~/zephyr
sudo cp /etc/fstab ~/zephyr
sudo cp /etc/X11/xorg.conf ~/zephyr
sudo cp /etc/profile ~/zephyr
