#!/usr/bin/python3
# run script from folder of regexpr.pas from https://github.com/andgineer/TRegExpr
# to create atsynedit_regexpr.pas

import sys
import os

def rep(l, s1, s2, needed=True):
    for (i, s) in enumerate(l):
        if s==s1:
            l[i]=s2
            return
    if needed:
        print('Cannot find required line: "'+s1+'"')
        sys.exit()

fn = os.path.join(os.path.dirname(__file__), 'regexpr.pas')
fn2 = os.path.join(os.path.dirname(__file__), 'atsynedit_regexpr.pas')

if not os.path.isfile(fn):
    print('File not found: regexpr.pas')
    sys.exit()
    
l = open(fn).read().split('\n')
if 'unit regexpr;' in l[0]:
    l[0] = 'unit atsynedit_regexpr;'
else:
    print('Cannot find unit name "regexpr"')
    sys.exit()
    
rep(l, '  regexpr_unicodedata;',
       '  atsynedit_unicodedata;')
rep(l, '{$I regexpr_compilers.inc}', 
       '//{$I regexpr_compilers.inc}')
rep(l, '{ off $DEFINE UnicodeEx} // Support Unicode >0xFFFF, e.g. emoji, e.g. "." must find 2 WideChars of 1 emoji',
       '{$DEFINE UnicodeEx}')

with open(fn2, 'w') as f:
    f.write('\n'.join(l))
    
print('File created:', fn2)
