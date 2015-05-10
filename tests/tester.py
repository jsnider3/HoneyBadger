import os
import subprocess
import unittest

class Tests(unittest.TestCase):

  def run_HB(self, fname):
    return os.popen("../HB " + fname).readlines()    

  def cat(self, fname):
    f = open(fname)
    lines = f.readlines()
    f.close()
    return lines
  
  def test_0(self):
    assert(self.run_HB("input/test0.in") == self.cat("output/test0.out"))

  def test_10(self):
    assert(self.run_HB("input/test10.in") == self.cat("output/test10.out"))

  def test_11(self):
    assert(self.run_HB("input/test11.in") == self.cat("output/test11.out"))

  def test_12(self):
    assert(self.run_HB("input/test12.in") == self.cat("output/test12.out"))

  def test_13(self):
    assert(self.run_HB("input/test13.in") == self.cat("output/test13.out"))

  def test_14(self):
    assert(self.run_HB("input/test14.in") == self.cat("output/test14.out"))

  def test_15(self):
    assert(self.run_HB("input/test15.in") == self.cat("output/test15.out"))

  def test_16(self):
    assert(self.run_HB("input/test16.in") == self.cat("output/test16.out"))

  def test_17(self):
    assert(self.run_HB("input/test17.in") == self.cat("output/test17.out"))

  def test_19(self):
    assert(self.run_HB("input/test19.in") == self.cat("output/test19.out"))

  def test_1(self):
    assert(self.run_HB("input/test1.in") == self.cat("output/test1.out"))

  def test_2(self):
    assert(self.run_HB("input/test2.in") == self.cat("output/test2.out"))

  def test_3(self):
    assert(self.run_HB("input/test3.in") == self.cat("output/test3.out"))

  def test_4(self):
    assert(self.run_HB("input/test4.in") == self.cat("output/test4.out"))

  def test_5(self):
    assert(self.run_HB("input/test5.in") == self.cat("output/test5.out"))

  def test_6(self):
    assert(self.run_HB("input/test6.in") == self.cat("output/test6.out"))

  def test_7(self):
    assert(self.run_HB("input/test7.in") == self.cat("output/test7.out"))

  def test_9(self):
    assert(self.run_HB("input/test9.in") == self.cat("output/test9.out"))

if __name__ == '__main__':
  unittest.main()
