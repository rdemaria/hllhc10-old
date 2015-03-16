#!/usr/bin/python

from pyoptics import *

from numpy import *

import sys

fn1=sys.argv[1] # previous squeeze point (iii)
fn2=sys.argv[2] # next squeeze point (iii+1)
fn3=sys.argv[3] # intermediate squeeze point with interpolation factor ttt
ttt=float(sys.argv[4])/100 # = ttt

out={}
for beam in ['b1','b2']:
    t1=optics.open(fn1.replace('b1',beam))
    t2=optics.open(fn2.replace('b1',beam))
    t3=optics.open(fn3.replace('b1',beam))
    idx1=where(t1//'IP3')[0]
    idx2=where(t2//'IP4')[0]
    for col in 'betx bety'.split():
        ref=t2[col]*ttt+t1[col]*(1-ttt) # betx/y assuming linear interpolation between iii and iii+1
        val=t3[col] # betx/y obtained
        err=abs(1-ref/val) # difference from betx/y to linear interpolation
        out['%s_%serr'%(col,beam)]=err.max() # maximum beta
        col3=col.replace('bet','bet34')
        out['%s_%serr'%(col3,beam)]=err[idx1:idx2].max() # maximum beta between IP3 and IP4
    for col in 'mux muy dx dy'.split():
        ref=t2[col]*ttt+t1[col]*(1-ttt)
        val=t3[col]
        out['%s_%serr'%(col,beam)]=abs(val-ref).max() # maximum mux/y, dx/y
    for col in 'x y'.split():
        ref=t2[col]*ttt+t1[col]*(1-ttt)
        val=t3[col]
        out['co%s_%serr'%(col,beam)]=abs(val-ref).max() # maximum orbit deviation x,y
    for col in 'q1 q2 dq1 dq2'.split():
        ref=t2.param[col]*ttt+t1.param[col]*(1-ttt)
        val=t3.param[col]
        out['%s_%serr'%(col,beam)]=abs(val-ref) # tune/chroma change 
for row in map(None, *([iter(sorted(out.keys()))]*4)):
    for col in row:
      print "%-10s=%20.14f;"%(col,out[col])

