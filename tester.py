import os
import subprocess
import unittest

INDIR = "tests/input/"
OUTDIR = "tests/output/"

class Tests(unittest.TestCase):

  def run_HB(self, fname):
    return os.popen("./HB " + fname).readlines()

  def cat(self, fname):
    f = open(fname)
    lines = f.readlines()
    f.close()
    return lines

  def dotest(self, fname):
    assert(self.run_HB(INDIR+fname+".hnb") == self.cat(OUTDIR+fname+".out"))

  def test_arith(self):
    self.dotest("arith")
    self.dotest("arith2")
    self.dotest("div")

  def test_func(self):
    self.dotest("func1")
    self.dotest("func2")

  def test_scope(self):
    self.dotest("scope")
    self.dotest("scope2")

  def test_fac(self):
    self.dotest("fac")

  def test_16(self):
    self.dotest("test16")

  def test_helloworld(self):
    self.dotest("helloworld")

  def test_while(self):
    self.dotest("while")

  def test_if(self):
    self.dotest("ifor")
    self.dotest("if2")
    self.dotest("if3")

  def test_casts(self):
    self.dotest("boolcasts")

  def test_seq(self):
    self.dotest("seq")

  def test_arr(self):
    self.dotest("index")
    self.dotest("set")
    self.dotest("len")

  def test_concat(self):
    self.dotest("concat")

  def test_9(self):
    self.dotest("test9")

  def test_qsort(self):
    self.dotest("qsort")

  def test_args(self):
    self.dotest("argOrder")
    self.dotest("multiarg")

if __name__ == '__main__':
  unittest.main()
