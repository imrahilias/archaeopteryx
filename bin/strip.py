#!/usr/bin/python
import sys

def strip(s):
    if isinstance(s, str):
        s = s.decode('utf-8')
    chars = []
    for i in s:
        if ord(i) > 0x7f:
            chars.append(u'?')
        else:
            chars.append(i)
    return u''.join(chars)

if __name__ == '__main__':
    print strip(sys.argv[1])
