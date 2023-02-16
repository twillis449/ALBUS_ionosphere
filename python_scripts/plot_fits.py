from __future__ import (print_function)
import matplotlib.pyplot as plt
from astropy.visualization import astropy_mpl_style
plt.style.use(astropy_mpl_style)
from astropy.io import fits
from scipy import interpolate
import numpy

image_file = 'iono_RM.FITS'
image_file = 'iono_STEC.FITS'
fits.info(image_file)
image_data = fits.getdata(image_file, ext=0)
print(image_data.shape)
size = image_data.shape[1]
size0 = image_data.shape[0]

new_image = numpy.zeros((size, size), numpy.float32)
print(new_image.shape)
x = numpy.arange(0, size0)
print 'x.shape', x.shape
print 'x', x
step_size = float(size0-1)/size
print 'step_size', step_size
for i in range(size):
  xnew = numpy.arange(0,size0-1,step_size)
# print 'xnew.shape', xnew.shape
# print 'xnew', xnew
  y = image_data[:,i]
# print 'y.shape', y.shape
  f = interpolate.interp1d(x, y)
  new_image[:,i] = f(xnew)   # use interpolation function returned by `interp1d`
#plt.figure()
plt.xlabel('time (5 minute steps)')
plt.ylabel('elevation Sequence north to south in 0.6 deg steps')
plt.imshow(new_image)
plt.colorbar()
plt.show()
