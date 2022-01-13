import netCDF4 as nc
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import gridspec
import re

class PIFreader:
    # A python class that reads electric field and particle position, velocity, and acceleration
    # output from the corresponding netcdf file
    # Contains a method to plot this data

    # Attributes
    def __init__(self,file_name):

        # File Names
        self.file_name = file_name
        self.open_file = "../data/" + file_name +  ".pif"
        self.img_file_name = "../Figures/" + file_name + ".png"

        # Load netcdf file
        self.dat = nc.Dataset(self.open_file, "r", format="NETCDF4")
    
        # Output Data
        # Electric field and charge density
        self.Ex = self.dat["E_x"][:]
        self.Ey = self.dat["E_y"][:]
        self.rho = self.dat["ChargeDensity"][:] 

        # Axes
        self.time = self.dat["t_axis"][:]
        self.x = self.dat["x_axis"][:]
        self.y = self.dat["y_axis"][:]

        # Particle positions, velocities, and accelerations
        self.positions = self.dat["Positions"][:]
        self.velocities = self.dat["Velocities"][:]
        self.accelerations = self.dat["Accelerations"][:]
    
    # Plotting
    def generate_plot(self):
        # General
        f = plt.figure(figsize=(20,10), dpi=400)
        f.suptitle(self.file_name)
        gs = gridspec.GridSpec(1,2, width_ratios=[2,1])

        # Subplot 1: Ex
        ax1 = f.add_subplot(531)
        img1 = ax1.imshow(self.Ex, extent=(-1,1,-1,1))
        ax1.set_title("Electric Field - Ex")
        ax1.set_xlabel("x")            
        ax1.set_ylabel("y")
        f.colorbar(img1)

        # Subplot 2: Ey
        ax2 = f.add_subplot(532)
        img2 = ax2.imshow(self.Ey, extent=(-1,1,-1,1))
        ax2.set_title("Electric Field - Ey")
        ax2.set_xlabel("x")            
        ax2.set_ylabel("y")
        f.colorbar(img2)

        # Subplot 3: Charge Density
        ax3 = f.add_subplot(533)
        img3 = ax3.imshow(self.rho, extent=(-1,1,-1,1))
        ax3.set_title("Electric Field - Charge Density")
        ax3.set_xlabel("x")            
        ax3.set_ylabel("y")
        f.colorbar(img3)

        # Subplot 4: X & Y Position vs time
        ax4 = f.add_subplot(534)
        ax4.plot(self.time, self.positions[0], linewidth=1, c="blue", label="X")
        ax4.plot(self.time, self.positions[1], linewidth=1, c="red", label="Y")
        ax4.set_title("Particle Positions vs. Time")
        ax4.set_xlabel("time")
        ax4.set_ylabel("Position")
        ax4.legend()

        # Subplot 5: Velocity vs time
        ax5 = f.add_subplot(535)
        ax5.plot(self.time, self.velocities[0], linewidth=1, c="blue", label="X")
        ax5.plot(self.time, self.velocities[1], linewidth=1, c="red", label="Y")
        ax5.set_title("Particle Velocities vs. Time")
        ax5.set_xlabel("time")
        ax5.set_ylabel("Velocity")
        ax5.legend()

        # Subplot 6: Accleration vs time
        ax6 = f.add_subplot(536)
        ax6.plot(self.time, self.accelerations[0], linewidth=1, c="blue", label="X")
        ax6.plot(self.time, self.accelerations[1], linewidth=1, c="red", label="Y")
        ax6.set_title("Particle Accelerations vs. Time")
        ax6.set_xlabel("time")
        ax6.set_ylabel("Acceleration")
        ax6.legend()

        # Subplot 7: Y Position vs X Position
        ax7 = f.add_subplot(537)
        ax7.scatter(self.positions[0], self.positions[1], s=0.5)
        ax7.set_title("Particle Y Position vs. X Position")
        ax7.set_xlabel("X")
        ax7.set_ylabel("Y")


        # Adjust spacing between subplots
        plt.subplots_adjust(left=0.1, bottom=-0.5, right=0.9, top=0.9, wspace=0.3, hspace=0.5)

        #plt.show()
        f.savefig(self.img_file_name, bbox_inches="tight")




