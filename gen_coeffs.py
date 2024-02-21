import os
import sys
import pdb

if not (os.path.abspath("../../thesdk") in sys.path):
    sys.path.append(os.path.abspath("../../thesdk"))

from thesdk import *
from URC_toolkit import *

class coefficient_generator():
    @property
    def _classfile(self):
        return os.path.dirname(os.path.realpath(__file__)) + "/"+__name__

    def __init__(self, *arg):
        """generator parameters and attributes

        """
        self.toolkit = URC_toolkit()

if __name__ == "__main__":
    import argparse
    
    gen = coefficient_generator()
    print("Generating coefficients for halfband filters")
    hb1_H, hb2_H, hb3_H = gen.toolkit.generate_uHfiles(f"{os.path.dirname(os.path.realpath(__file__))}/")
    cic3 = gen.toolkit.CIC.calc_hwCIC(gen.toolkit.gen_simple_signal("Impulse", 1, 2500))
    gen.toolkit.plot_coeff_fft(hb1_H, hb2_H, hb3_H, cic3)
    input("---\nPress enter to continue to generation\n---\n")

