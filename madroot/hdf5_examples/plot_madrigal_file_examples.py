"""This python script gives two simple examples of plotting radar data.  

Assumes the Madrigal file has an array layout with independent parameter range, and 2D parameter ne.  
For the line plot, assumes file has 1D parameter systmp.

Written by Bill Rideout - brideout@mit.edu

$Id: plot_madrigal_file_examples.py 7371 2021-04-22 19:29:58Z brideout $
"""

# constants
test_file = '/Users/brideout/Downloads/mlh210107g.001.hdf5'

import matplotlib.pyplot, h5py

"""This particular file has two array layouts, divided by single pulse and alternating code. 
We will plot the alternating code (mdtyp == 97). If there were only one array layout, the group 
"Array with kinst=32.0 and mdtyp=97.0 and pl=0.00048 " would be skipped when defining arrayData.
"""

f = h5py.File(test_file, 'r')
arrayData = f['Data']['Array Layout']['Array with kinst=32.0 and mdtyp=97.0 and pl=0.00048 ']
timestamps = arrayData['timestamps']
ranges = arrayData['range']
ne = arrayData['2D Parameters']['ne']
systmp = arrayData['1D Parameters']['systmp']

# We now have all the data - the rest of the code is just plotting.

matplotlib.pyplot.pcolor(timestamps, ranges, ne)
matplotlib.pyplot.xlabel('Time in unix seconds')
matplotlib.pyplot.ylabel('Range in km')
matplotlib.pyplot.colorbar()
matplotlib.pyplot.title('Electron Density')
print('First display a pcolor plot of Ne - close plot to see next example')
matplotlib.pyplot.show()

matplotlib.pyplot.plot(timestamps, systmp)
matplotlib.pyplot.xlabel('Time in unix seconds')
matplotlib.pyplot.ylabel('System temp (K)')
matplotlib.pyplot.title('Millstone Hill System Temperature')
print('Note that the plot shows Cygnus passing through the beam, used for gain monitoring')
matplotlib.pyplot.show()
