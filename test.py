#!/usr/bin/env python

from earley import *
import sys
import os
import re

def check_line(grammar, line):
  try:
    parse(grammar, line)
    return True
  except ValueError as e:
    return False

def check_file(grammar, lines, verdict = True):
  bad_lines = []
  for line in lines:
    if check_line(grammar, line) != verdict:
      bad_lines.append(line)
  return len(bad_lines) == 0, bad_lines 

def readlines(fname):
  f = open(fname)
  lines = f.read().splitlines()
  f.close()
  return lines

if __name__ == "__main__":
  
  grammar_file_name = 'grammar.txt'
  if len(sys.argv) <= 1:
    print "reading %s"%grammar_file_name
  else:
    grammar_file_name = sys.argv[1]

  g = load_grammar(open(grammar_file_name)) 

  for fname in os.listdir('.'):
    if not os.path.isfile(fname):
      continue
    result, bad_lines = True, []
    if re.match('.*_ok$', fname):
      result, bad_lines = check_file(g, readlines(fname), True)
    elif re.match('.*_fail$', fname):
      result, bad_lines = check_file(g, readlines(fname), False)
    if not result:
      print "%s Fail: "%fname, bad_lines


