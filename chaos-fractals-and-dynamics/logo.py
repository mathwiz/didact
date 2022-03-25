from turtle import *

def exec(cmds):
  for line in cmds.split('\n'):
    if len(line.strip()) > 0:
      process(line)
  
def process(line):
  cmd, args = parse(line)
  #examine(cmd, args)
  doLogo(cmd, args)

def doLogo(cmd, args):
  ns = [ float(arg) for arg in args if arg ]
  if cmd=='home':
    home()
  elif cmd=='clear':
    clear()
  elif cmd=='pu':
    pu()
  elif cmd=='pd':
    pd()
  elif cmd=='fd':
    fd(ns[0])
  elif cmd=='bk':
    bk(n[0])
  elif cmd=='lt':
    lt(n[0])
  elif cmd=='rt':
    rt(n[0])
  elif cmd=='goto':
    goto(n[0], n[1])
  else:
    print("Unknown cmd:[%s]" % (cmd))

def parse(line):
  syms = [ tok for tok in line.split(' ') if len(tok) > 0 ]
  return syms[0], syms[1:] if len(syms) > 1 else []

def examine(cmd, args):
  print("cmd*%s* arg*%s*" % (cmd, args))
