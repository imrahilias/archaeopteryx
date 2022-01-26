# on system maintenance and stuff
`https://wiki.archlinux.org/index.php/System_maintenance`
`https://wiki.archlinux.org/index.php/Pacman/Tips_and_tricks`

# backup
the following command can be used to back up the local pacman database:
`tar -cjf pacman_database_$(date +%y%m%d).tar.bz2 /var/lib/pacman/local`

# list
keeping a list of explicitly installed packages can be useful to speed
up installation on a new system:
`pacman -Qqe > installed_packages_$(date +%y%m%d)`

# list aur
keeping a list of manually installed packages (aur):
`pacman -Qm > installed_packages_manually_$(date +%y%m%d)`

# shrink cache
the paccache script, provided within the pacman-contrib package,
deletes all cached versions of installed and uninstalled packages,
except for the most recent 3, by default:
`paccache -r`

# remove unused
to remove all the cached packages that are not currently installed,
and the unused sync database, execute:
`pacman -Sc`

# purge cache
to remove all files from the cache, use the clean switch twice, this
is the most aggressive approach and will leave nothing in the cache
folder:
`pacman -Scc`

# orphans
delete orphans and dropped packages:
`pacman -Rns $(pacman -Qtdq)`

# lost
script that identifies files not owned by any package:
`lostfiles strict > lost_files_$(date +%y%m%d)`

# broken
to quickly list all the broken symlinks of your system, use:
`find -xtype l -print > broken_links_$(date +%y%m%d)`

# reflect
filter the 10 newest and fastest rsync compatible mirrors:
`reflector -p rsync -f 10 -l 10 --sort rate`

# fastest
now add the 10 newest https mirrors:
`reflector -p https -f 10 -l 10 --sort rate --save /etc/pacman.d/mirrorlist`
