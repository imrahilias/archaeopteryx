# create archives
tar cz my_large_file_1 my_large_file_2 | split -b 1024MiB - myfiles_split.tgz_

# uncompress
cat myfiles_split.tgz_* | tar xz

this solution avoids the need to use an intermediate large file when
(de)compressing. use the tar -c option to use a different directory
for the resulting files. btw if the archive consists from only a
single file, tar could be avoided and only gzip used:

# create archives
gzip -c my_large_file | split -b 1024MiB - myfile_split.gz_

# uncompress
cat myfile_split.gz_* | gunzip -c > my_large_file

# windows
it can be reassembled on a windows machine using:
copy /b file1 + file2 + file3 + file4 filetogether