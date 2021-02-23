#!/bin/zsh
# Rename files by replacing Unix-unfriendly characters.

usage () {
    cat <<EOF
usage: $0 [OPTIONS] [--] [FILE [FILE...]]
Rename files by replacing Unix-unfriendly characters.

Options:
 -p N              preserve last N dots in filename, or keep all
                   dots if N < 0 (default: 1)
       --help      show this help and exit
EOF
}

error () {
    printf "%s\n" "$1" 1>&2
}

delete_chars="()[]{}*?!^~%\\\<>&\$#|'\`\"@=+"
replace_chars=" _.,:;-"

unixify_string () (
    printf '%s\n' "$1" \
        | tr -d "$delete_chars" \
        | tr -s "$replace_chars" _ \
        | to_lower \
        | sed 's/^-\(.\)/\1/; s/\(.\)-$/\1/;' \
        | umlaut
)

to_lower () {
    sed 's/.*/\L&/'
}

umlaut () {
    sed 's/ä/ae/g; s/ö/oe/g; s/ü/ue/g; s/ß/sz/g;'
}

split () (
    # split '.x.x.x.x'  0 -> '/x.x.x.x.x
    # split '.x.x.x.x'  1 -> '/x.x.x.x/x
    # split '.x.x.x.x'  2 -> '/x.x.x/x/x
    # split '.x.x.x.x' -1 -> '/x/x/x/x/x
    nf=$(printf '%s\n' "$1" | tr -d -C . | wc -c)
    if [ $2 -lt 0 ]; then
        keep=0
    else
        keep=$((nf-$2))
    fi
    IFS=. i=0 out= sep=
    for part in $1; do
        out="$out$sep$part"
        if [ -z "$out" -o $i -ge $keep ]; then
            sep=/
        else
            sep=.
        fi
        i=$(($i+1))
    done
    printf '%s\n' "$out"
)

unixify () (
    IFS=/ out= sep=
    for part in $(split "$1" $2); do
        out="$out$sep$(unixify_string "$part")"
        sep=.
    done
    printf '%s\n' "$out"
)

rename_maybe () (
    dir="$(dirname "$1")"
    name="$(basename "$1")"
    newname="$(unixify "$name" $2)"
    if [ "$newname" != "$name" ]; then
        mv -i "$dir/$name" "$dir/$newname"
    fi
)

# command line arguments

short_opts=p:
long_opts=help

args="$(LC_ALL=C getopt -n "$0" -s sh -o $short_opts -l $long_opts -- "$@")"
if [ $? -eq 0 ]; then
    eval set -- "$args"
else
    exit 1
fi

p=
while [ $# -gt 0 ]; do
    case "$1" in
        --help)
            usage; exit 0 ;;
        -p)
            p="$2"; shift
            if ! [ "$p" -eq "$p" ] 2> /dev/null; then
                error "$0: option requires integer argument -- 'p'"
                exit 1
            fi ;;
        --)
            shift; break ;;
        -*)
            error "$0: illegal option -- '$1'"
            exit 1 ;;
        *)
            break
    esac
    shift
done

# defaults
p=${p:-1}

# echo p=$p
# echo "$@"
# echo n=$#
# exit

if [ $# -lt 1 ]; then
    error "$0: required non-option argument missing"
    exit 1
fi

for file in "$@"; do
    rename_maybe "$file" $p
done
