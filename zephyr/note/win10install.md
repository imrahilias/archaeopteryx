# 1) try
~/troika/archive/projects/computer @ sudo dd if=WA61501_de.iso of=/dev/sde bs=1M

did create a hdf5 volume, including content, but didnt show up as uefi
in bios, and didnt boot.

# 2) gparted gpt table + ntfs + c&p + sync + boot/esp flag

did create a ntfshdf5 volume, including content, but didnt show up as
uefi in bios, and didnt boot.

# 3) gparted gpt table + ntfs + c&p + sync + msftdata flag same.

# 4) gparted gpt table + fat32 + c&p + sync