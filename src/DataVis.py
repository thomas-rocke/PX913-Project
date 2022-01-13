# NetCDF Data loading and Data Visualisation
import netCDF4 as nc
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import gridspec
from PIFreader import PIFreader

single = PIFreader("Run(Single, nx=100, ny=100)")

single.generate_plot()

double = PIFreader("Run(Double, nx=100, ny=100)")

double.generate_plot()

single = PIFreader("Run(Null, nx=100, ny=100)")

single.generate_plot()