#!/bin/zsh
# extract .fit to csv &  gpx
#
for file in "$@"; do
    gpsbabel -i garmin_fit -f "$file" -o gpx -F $file.gpx # make gpx
    java -jar ~/bin/fitcsvtool.jar -b $file $file # make csv
done
