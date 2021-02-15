# full backup, no specials, just files

rsync -hPrv troika/photos/ knox

-h human readable
-P progress & partial
-r rekursive
-v verbose

trailing slash! 'source/ destination' copies content of source folder and doesnt create a folder called :"source" with content in destination.


# incremental ordered backup, no specials, just files

rsync -ahv --delete source/ destination/current --link-dest=../now

now beeing a soft link to the initial backup