import os
import sys
import pdb

if not (os.path.abspath("../../thesdk") in sys.path):
    sys.path.append(os.path.abspath("../../thesdk"))

from thesdk import *

from dsp_toolkit import *
from urc.urc_model import *

class coefficient_generator():
    @property
    def _classfile(self):
        return os.path.dirname(os.path.realpath(__file__)) + "/"+__name__

    def __init__(self, *arg):
        """generator parameters and attributes

        """
        self.dsp_tk = dsp_toolkit()
        self.urc = urc_model()

if __name__ == "__main__":
    import argparse
    
    gen = coefficient_generator()
    print("Generating coefficients for urc subfilters")
    hb1_H, hb2_H, hb3_H = gen.urc.HB1.generate_Hfiles(f"{os.path.dirname(os.path.realpath(__file__))}/f2_universal/hb_universal/", gen.urc.HB1, gen.urc.HB2, gen.urc.HB3)
    #cic3 = gen.urc.CIC3.hw_interpolation(gen.dsp_tk.gen_simple_signal(singal_type="Impulse", 1, 250))
    #gen.dsp_tk.plot_coeff_fft([hb1_H, hb2_H, hb3_H, cic3])
    #input("---\nPress enter to continue to generation\n---\n")

