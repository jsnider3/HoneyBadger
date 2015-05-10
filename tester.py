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
    assert(self.run_HB(INDIR+fname+".in") == self.cat(OUTDIR+fname+".out"))
  
  def test_0(self):
    self.dotest("test0")

  def test_10(self):
    self.dotest("test10")

  def test_11(self):
    self.dotest("test11")

  def test_12(self):
    self.dotest("test12")

  def test_13(self):
    self.dotest("test13")

  def test_14(self):
    self.dotest("test14")

  def test_15(self):
    self.dotest("test15")

  def test_16(self):
    self.dotest("test16")

  def test_17(self):
    self.dotest("test17")

  def test_19(self):
    self.dotest("test19")

  def test_1(self):
    self.dotest("test1")

  def test_2(self):
    self.dotest("test2")

  def test_21(self):
    self.dotest("test21")

  def test_22(self):
    self.dotest("test22")

  def test_3(self):
    self.dotest("test3")

  def test_4(self):
    self.dotest("test4")

  def test_5(self):
    self.dotest("test5")

  def test_6(self):
    self.dotest("test6")

  def test_7(self):
    self.dotest("test7")

  def test_9(self):
    self.dotest("test9")

if __name__ == '__main__':
  unittest.main()
