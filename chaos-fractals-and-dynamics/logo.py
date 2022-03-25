from turtle import *

def exec(cmds):
  for line in cmds.split('\n'):
    if len(line.strip()) > 0:
      process(line)
  
def process(line):
  cmd, arg = parse(line)
  if arg:
    locals()[cmd](arg)
  else:
    locals()[cmd]()

def parse(line):
  syms = [ tok for tok in line.split(' ') if len(tok) > 0 ]
  return syms[0], syms[1] if len(syms) > 1 else None

def examine(cmd, arg):
  print("cmd*%s* arg*%s*" % (cmd, arg))
