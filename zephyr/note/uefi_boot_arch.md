EFISTUB write uefi boot entry
https://wiki.archlinux.org/index.php/EFISTUB
https://wiki.archlinux.org/index.php/Kernel_mode_setting
https://wiki.archlinux.org/index.php/NVIDIA#DRM_kernel_mode_setting
https://wiki.archlinux.org/index.php/Unified_Extensible_Firmware_Interface#UEFI_Shell

# efistub

## efibootmgr --create --disk /dev/sda --part 1 --write-signature --label "Arch Linux KMS" --loader /EFI/arch/vmlinuz-linux --unicode 'root=UUID=ede305dc-3791-4e5b-8ba1-cafa2e0211e2 rw initrd=/EFI/arch/intel-ucode.img rw initrd=/EFI/arch/initramfs-linux.img rw nvidia-drm.modeset=1'


# working efistub setup

~ @ efibootmgr                                                                                      14:49
BootCurrent: 0000
Timeout: 1 seconds
BootOrder: 0000,0002,0005,0004
Boot0000* Arch Linux KMS
Boot0002* Windows Boot Manager
Boot0004* Hard Drive 
Boot0005* UEFI: TDKMediaTrans-It Drive PMAP
~ @ efibootmgr -v                                                                                   14:49
BootCurrent: 0000
Timeout: 1 seconds
BootOrder: 0000,0002,0005,0004
Boot0000* Arch Linux KMS	HD(1,GPT,62e194b5-d973-4c5a-b1d4-00f3031151a2,0x800,0x100000)/File(\EFI\arch\vmlinuz-linux)r.o.o.t.=.U.U.I.D.=.e.d.e.3.0.5.d.c.-.3.7.9.1.-.4.e.5.b.-.8.b.a.1.-.c.a.f.a.2.e.0.2.1.1.e.2. .r.w. .i.n.i.t.r.d.=./.E.F.I./.a.r.c.h./.i.n.t.e.l.-.u.c.o.d.e...i.m.g. .r.w. .i.n.i.t.r.d.=./.E.F.I./.a.r.c.h./.i.n.i.t.r.a.m.f.s.-.l.i.n.u.x...i.m.g. .r.w. .n.v.i.d.i.a.-.d.r.m...m.o.d.e.s.e.t.=.1.
Boot0002* Windows Boot Manager	HD(1,GPT,62e194b5-d973-4c5a-b1d4-00f3031151a2,0x800,0x100000)/File(\EFI\Microsoft\Boot\bootmgfw.efi)WINDOWS.........x...B.C.D.O.B.J.E.C.T.=.{.9.d.e.a.8.6.2.c.-.5.c.d.d.-.4.e.7.0.-.a.c.c.1.-.f.3.2.b.3.4.4.d.4.7.9.5.}....................
Boot0004* Hard Drive 	BBS(HD,,0x0)AMGOAMNO........o.S.a.m.s.u.n.g. .S.S.D. .8.4.0. .S.e.r.i.e.s....................A...........................>..Gd-.;.A..MQ..L.1.S.C.4.E.N.C.A.0.C.9.5.6.1. .F. . . . ......AMBOAMNO........o.S.a.m.s.u.n.g. .S.S.D. .8.4.0. .S.e.r.i.e.s....................A...........................>..Gd-.;.A..MQ..L.1.S.G.4.E.N.C.A.1.A.9.9.7.5. .A. . . . ......AMBOAMNO........o.S.A.M.S.U.N.G. .H.D.1.0.5.S.I....................A...........................>..Gd-.;.A..MQ..L.2.S.G.5.9.J.Z.0.0.B.9.0.6.8. . . . . . ......AMBOAMNO........o.T.O.S.H.I.B.A. .D.T.0.1.A.C.A.3.0.0....................A...........................>..Gd-.;.A..MQ..L. . . . . . . . . . .6. .Q.3.X.Y.Y.R.S.G......AMBOAMNO.........T.D.K.M.e.d.i.a.T.r.a.n.s.-.I.t. .D.r.i.v.e. .P.M.A.P....................A.............................L..Gd-.;.A..MQ..L.T.D.K.M.e.d.i.a.T.r.a.n.s.-.I.t. .D.r.i.v.e. .P.M.A.P......AMBO
Boot0005* UEFI: TDKMediaTrans-It Drive PMAP	PciRoot(0x0)/Pci(0x1c,0x1)/Pci(0x0,0x0)/USB(4,0)/HD(1,GPT,ce54371f-31bd-4f2a-a842-acedb6a82bbc,0x800,0x4e2000)AMBO
~ @

# uefi shell

it is possible to launch an efistub kernel from uefi shell as if it is
a normal uefi application. in this case the kernel parameters are
passed as normal parameters to the launched efistub kernel file.

## fs0:
## \vmlinuz-linux  -u root=UUID=ede305dc-3791-4e5b-8ba1-cafa2e0211e2 rw initrd=/initramfs-linux.img

to avoid needing to remember all of your kernel parameters every time,
you can save the executable command to a shell script such as
archlinux.nsh on your uefi system partition, then run it with:

## fs0:
## archlinux

# startup.nsh

the uefi shell specification 2.0 establishes that a script called
startup.nsh at the root of the esp partition will always be
interpreted and can contain arbitrary instructions; among those you
can set a bootloading line. make sure you mount the esp partition
on /boot and create a startup.nsh script that contains a kernel
bootloading line. for example:

## vmlinuz-linux rw root=/dev/sdX [rootfs=myfs] [rootflags=myrootflags] \
## [kernel.flag=foo] [mymodule.flag=bar] \
## [initrd=\intel-ucode.img] initrd=\initramfs-linux.img
 
this method will work with almost all uefi firmware versions you may
encounter in real hardware, you can use it as last resort. the script
must be a single long line. sections in brackets are optional and
given only as a guide. shell style linebreaks are for visual
clarification only. fat filesystems use the backslash as path
separator and in this case, the backslash declares the initramfs is
located in the root of the esp partition. only intel microcode is
loaded in the booting parameters line; amd microcode is read from disk
later during the boot process; this is done automatically by the
kernel.


# startup.nsh

## vmlinuz-linux -u root=UUID=ede305dc-3791-4e5b-8ba1-cafa2e0211e2 rw initrd=/EFI/arch/intel-ucode.img rw initrd=/EFI/arch/initramfs-linux.img rw nvidia-drm.modeset=1