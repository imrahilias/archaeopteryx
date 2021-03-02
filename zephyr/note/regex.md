# newlines

## many text processing tools, including sed, operate on the content of
## the line, excluding the newline character. the first thing sed does
## when processing a line is to strip off the newline at the end, then it
## executes the commands in the script, and it adds a final newline when
## printing out. so you won't be able to remove the newline with sed.

## to remove all the newlines, you can use tr instead:
tr -d '\n'


# regex

## ascending sed regexes (bre) in order to produce nice
## file/dir-names.

's/.*/\L&/' ... lowercase (including umlauts)

'y/äöü/aou/' ... translate all lower umlauts to ascii

's/ß/sz/g' ... sz is more than the one character it replaces

's/[^A-Za-z0-9.]/_/g' ...not alphanumeric nor dot

's/^\./?/' ... first dot at the beginning of the string gets '?'

's/\.\([a-z]*\)$/?\1/g' ... only the single dot who followed by 0 or more lower at the end of the string (file extension) gets replaced with ?

's/\./_/g' ... dots get underline

's/_*_/_/g' ... two or more underline (0 or more underline followed by 1 underline) get 1 underline

's/?/\./g' ... the only available '?' is either the hidden-dot, or the extensnion-dot (both valid dots) so get dots again

's/\s/_/g' ... space-like characters get underline

's/\W//g' ... only words

's/\w//g' ... only non-words (lowercase w!)

's/[.]/_/g' ... all dots get underline

s/\.[a-z]*$/?/g ... 1 dot followed by 0 or more lower at the end of the string (file extension) get '?'

's/^[^.]/?/g' ... every leading character, which is not a dot

's/^\.\([^.]\+\)/?\1/' ... every string with leading dot, with at least one following char not being dot


## problems

'+' is a valid dirname! (eg. 'lost+found')

files with leading '#' lose it, unless '#' is excluded from stripping (eg. '#tmp#')


## example (should give '.aoaaopu_nyik_es_oh_look_t_afile_txt_adf_dafh_rgsdh_h.txt')
echo '.aÖÄäöpüß\nyik?es_𝄞_oh_l o o k:\t:a file_火.txt..adf+dafH\\rGSDH>h.txt' | tr -d '\n' | sed 's/\s//g ; s/.*/\L&/ ; y/äöü/aou/ ; s/[^A-Za-z0-9.]/_/g ; s/^\./?/ ; s/\.\([a-z]*\)$/?\1/g ; s/\./_/g ; s/_*_/_/g ; s/?/\./g'
