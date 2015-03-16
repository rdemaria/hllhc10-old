#!/usr/bin/python

# create new squeeze table fn_new.tfs by selecting steps from previous fn.tfs squeeze table
# steps can be selected by e.g. beta function 'betxip5b1'
# the steps are either defined in a file or directly in this script (steps)
# example if steps are defined in this file: "toolkit/read_squeeze_steps.py 'slhc/squeeze/ir5_presqueeze.tfs' 'betxip5b1'
# example if steps are defined in input file steps.dat: "toolkit/read_squeeze_steps.py 'slhc/squeeze/ir5_presqueeze.tfs' 'betxip5b1' 'steps.dat'

from pyoptics import *
from numpy import *
import sys
import itertools

fn1=str(sys.argv[1]) # squeeze file e.g. ir5_presqueeze.tfs
fn2=str(sys.argv[2]) # parameter defining the squeeze steps, e.g. betxip5b1
ttot = tfsdata.open(fn1)
ssq  = fn2 
if len(sys.argv)>3:
  fn3=str(sys.argv[3]) # file with beta values, each beta value in one line
  fnsteps=open(fn3,'r')
  steps=[]
  for bb in fnsteps:
    steps.append(float(bb))
else:
  steps=[0.44,0.5,0.6,0.8]+[(ii+2.0)*0.5 for ii in xrange(9)]+[5.2,5.4,5.6,5.8,6.0]
steps=sorted(list(set(steps)))

def index(l,f):
#  return next((i for i in xrange(len(l)) if f(l[i])), None)
  return [i for i in xrange(len(l)) if f(l[i])]

def mk_feq(bb):
  def feq(x):
    if(abs(x-bb)<1.e-6): return True
    else: return False
  return feq 

idx = list(itertools.chain.from_iterable([ index(ttot[ssq],mk_feq(bb)) for bb in steps ]))

for k in ttot['col_names']:
  ttot[k] = ttot[k][idx]

tfsdata.save(ttot,'ir5_presqueeze_new.tfs')
