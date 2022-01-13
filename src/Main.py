import sys
import os
from PIFreader import PIFreader

#Constants
problems = ["Null", "Single", "Double"]

path = r"../data/"


# Read command line args
args = sys.argv

nx = args[1]

ny = args[2]

if (len(args) > 3) and (args[3] in problems): # Problem specified, and allowed
    probs_to_use = [args[3]]
else:
    probs_to_use = problems


# Go through each possible problem
for prob in probs_to_use:
    fname = f"Run({prob}, nx={nx}, ny={ny})"
    full_path = path + fname
    print(full_path)
    # Only attempt to update plots if files exist
    if os.path.exists(full_path):
        pass
        data = PIFreader(full_path)
        data.generate_plot()