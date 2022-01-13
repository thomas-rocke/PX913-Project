import sys
import os
from PIFreader import PIFReader
from DataVis import generate_plot

#Constants
problems = ["Null", "Single", "Double"]

path = r"../data/"


# Read command line args
args = sys.argv

nx = args[1]

ny = args[2]

# Go through each possible problem
for prob in problems:
    fname = f"Run({prob}, nx={nx}, ny={ny}).pif"
    full_path = path + fname

    # Only attempt to update plots if files exist
    if os.path.exists(full_path):
        data = PIFReader(full_path)
        generate_plot(data)