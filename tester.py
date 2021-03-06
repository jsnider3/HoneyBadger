import os
import subprocess
import unittest

IN = "tests/input/"
OUT = "tests/output/"

class Tests(unittest.TestCase):

  def run_HB(self, fname):

    lines = subprocess.check_output(["./HB", fname])
    lines = lines.split('\n')[:-1]
    return lines

  def cat(self, fname):
    f = open(fname)
    lines = f.readlines()
    lines = [line.rstrip('\n') for line in lines]
    f.close()
    return lines

  def dotest(self, fname):
    assert(self.run_HB(IN + fname + ".hnb") == self.cat(OUT + fname + ".out"))

  def test_args(self):
    self.dotest("argOrder")
    self.dotest("multiarg")

  def test_arith(self):
    self.dotest("arith")
    self.dotest("arith2")
    self.dotest("div")

  def test_arr(self):
    self.dotest("index")
    self.dotest("set")
    self.dotest("len")
    self.dotest("concat")

  def test_casts(self):
    self.dotest("boolcasts")

  def test_excepts(self):
    self.dotest("except")
    self.dotest("except1")
    self.dotest("trycatch")

  def test_func(self):
    self.dotest("func1")
    self.dotest("func2")
    self.dotest("func3")
    self.dotest("fac")

  def test_if(self):
    self.dotest("ifor")
    self.dotest("if2")
    self.dotest("if3")

  def test_print(self):
    self.dotest("helloworld")

  def test_qsort(self):
    self.dotest("qsort")

  def test_scope(self):
    self.dotest("scope")
    self.dotest("scope2")

  def test_seq(self):
    self.dotest("seq")

  def test_while(self):
    self.dotest("while")
    self.dotest("forloop")

  def test_9(self):
    self.dotest("test9")

  def test_16(self):
    self.dotest("test16")

  def test_dict(self):
    self.dotest("dict")

if __name__ == '__main__':
  unittest.main()
