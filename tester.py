import os
import subprocess
import unittest

INDIR = "tests/input/"
OUTDIR = "tests/output/"

class Tests(unittest.TestCase):

  def run_HB(self, fname):
    return os.popen("HB " + fname).readlines()    

  def cat(self, fname):
    f = open(fname)
    lines = f.readlines()
    f.close()
    return lines
  
  def test_0(self):
    assert(self.run_HB(INDIR+"test0.in") == self.cat(OUTDIR+"test0.out"))

  def test_10(self):
    assert(self.run_HB(INDIR+"test10.in") == self.cat(OUTDIR+"test10.out"))

  def test_11(self):
    assert(self.run_HB(INDIR+"test11.in") == self.cat(OUTDIR+"test11.out"))

  def test_12(self):
    assert(self.run_HB(INDIR+"test12.in") == self.cat(OUTDIR+"test12.out"))

  def test_13(self):
    assert(self.run_HB(INDIR+"test13.in") == self.cat(OUTDIR+"test13.out"))

  def test_14(self):
    assert(self.run_HB(INDIR+"test14.in") == self.cat(OUTDIR+"test14.out"))

  def test_15(self):
    assert(self.run_HB(INDIR+"test15.in") == self.cat(OUTDIR+"test15.out"))

  def test_16(self):
    assert(self.run_HB(INDIR+"test16.in") == self.cat(OUTDIR+"test16.out"))

  def test_17(self):
    assert(self.run_HB(INDIR+"test17.in") == self.cat(OUTDIR+"test17.out"))

  def test_19(self):
    assert(self.run_HB(INDIR+"test19.in") == self.cat(OUTDIR+"test19.out"))

  def test_1(self):
    assert(self.run_HB(INDIR+"test1.in") == self.cat(OUTDIR+"test1.out"))

  def test_2(self):
    assert(self.run_HB(INDIR+"test2.in") == self.cat(OUTDIR+"test2.out"))

  def test_3(self):
    assert(self.run_HB(INDIR+"test3.in") == self.cat(OUTDIR+"test3.out"))

  def test_4(self):
    assert(self.run_HB(INDIR+"test4.in") == self.cat(OUTDIR+"test4.out"))

  def test_5(self):
    assert(self.run_HB(INDIR+"test5.in") == self.cat(OUTDIR+"test5.out"))

  def test_6(self):
    assert(self.run_HB(INDIR+"test6.in") == self.cat(OUTDIR+"test6.out"))

  def test_7(self):
    assert(self.run_HB(INDIR+"test7.in") == self.cat(OUTDIR+"test7.out"))

  def test_9(self):
    assert(self.run_HB(INDIR+"test9.in") == self.cat(OUTDIR+"test9.out"))

if __name__ == '__main__':
  unittest.main()
