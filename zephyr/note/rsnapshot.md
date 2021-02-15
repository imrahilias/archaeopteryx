# rsnapshot
## do NOT enter comments after elements in rsnapshot.conf, it kills rsync!

/etc/rsnapshot.conf
https://wiki.archlinux.org/index.php/System_maintenance
https://wiki.archlinux.org/index.php/Rsync
https://wiki.archlinux.org/index.php/Rsnapshot
https://linux.die.net/man/1/rsync
https://wiki.archlinux.org/index.php/Cron

# configtest

when you have made all your changes, you should verify that the config
file is syntactically valid, and that all the supporting programs are
where you think they are. to do this, run rsnapshot with the
configtest argument: if all is well, it should say syntax ok. if
there's a problem, it should tell you exactly what it is. make sure
your config file is using tabs and not spaces, etc.

## rsnapshot configtest

# test once

the final step to test your configuration is to run rsnapshot in test
mode. this will print out a verbose list of the things it will do,
without actually doing them. to do a test run, run this command:

## rsnapshot -t hourly

# perform manually once
systemctl start rsnapshot@hourly

# automate and start immediately
systemctl enable --now rsnapshot-hourly.timer


# automation
now that you have your config file set up, it's time to set up
rsnapshot to be run automatically.


# service file
/etc/systemd/system/rsnapshot@.service

[Unit]
Description=rsnapshot (%I) backup

[Service]
Type=oneshot
Nice=19
IOSchedulingClass=idle
ExecStart=/usr/bin/rsnapshot %I


# monthly timer unit
/etc/systemd/system/rsnapshot-monthly.timer

[Unit]
Description=rsnapshot monthly backup

[Timer]
# Run once per month at 12:00 UTC
OnCalendar=*-*-01 12:00:00
OnBootSec=10m
Persistent=true
Unit=rsnapshot@monthly.service

[Install]
WantedBy=timers.target


# weekly timer unit
/etc/systemd/system/rsnapshot-weekly.timer

[Unit]
Description=rsnapshot weekly backup

[Timer]
# Run once per week on Monday at 13:00, hopefully after monthly backup finished 
OnCalendar=Mon *-*-01 13:00:00
OnBootSec=20m
Persistent=true
Unit=rsnapshot@weekly.service

[Install]
WantedBy=timers.target


# daily timer unit
/etc/systemd/system/rsnapshot-daily.timer

[Unit]
Description=rsnapshot daily backup

[Timer]
# 05:30 is the clock time when to start it, hopefully after weekly & monthly backups finished 
OnCalendar=14:00
OnBootSec=30m
Persistent=true
Unit=rsnapshot@daily.service

[Install]
WantedBy=timers.target


# hourly timer unit
/etc/systemd/system/rsnapshot-hourly.timer

[Unit]
Description=rsnapshot hourly backup

[Timer]
# Time to wait after booting before we run first time, hopefully after daily & weekly & monthly backups finished 
OnBootSec=1m
# Time between running each consecutive time
OnUnitActiveSec=1h
Unit=rsnapshot@hourly.service

[Install]
WantedBy=timers.target





