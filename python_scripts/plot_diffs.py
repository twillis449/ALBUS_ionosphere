from __future__ import (print_function)
import matplotlib.pyplot as plt
from astropy.visualization import astropy_mpl_style
plt.style.use(astropy_mpl_style)
from astropy.io import fits
from scipy import interpolate
import numpy

image_file = 'iono_STEC.FITS'
image_file_albus = 'iono_STEC_ALBUS.FITS'
image_file = 'iono_RM.FITS'
image_file = 'iono_STEC_EMM_2016.FITS'
#image_file = 'iono_STEC_EMM_2015.FITS'
image_file_albus = 'iono_STEC_ALBUS_2016.FITS'
image_file = 'iono_RM_EMM_2016.FITS'
image_file_albus = 'iono_RM_ALBUS_2016.FITS'
image_file = 'iono_RM_EMM_2015.FITS'
image_file_albus = 'iono_RM_ALBUS_2015.FITS'
image_file1 = 'iono_RM_EMM.FITS'
image_file_albus1 = 'iono_RM_ALBUS.FITS'
image_file = 'iono_RM_EMM_2017_geocentric.FITS'
image_file_albus = 'iono_RM_ALBUS_2017_geocentric.FITS'

fits.info(image_file)
fits.info(image_file_albus)
image_data = fits.getdata(image_file, ext=0)
image_data_albus = fits.getdata(image_file_albus, ext=0)
image_data1 = fits.getdata(image_file1, ext=0)
image_data_albus1 = fits.getdata(image_file_albus1, ext=0)
print('image_data shape',image_data.shape)
size = image_data.shape[1]
size0 = image_data.shape[0]

image_data = image_data - image_data_albus
image_data1 = image_data1 - image_data_albus1
image_data = image_data - image_data1
#image_data = image_data_albus
new_image = numpy.zeros((size, size), numpy.float32)
print(new_image.shape)
x = numpy.arange(0, size0)
x1 = (numpy.arange(0, size) * 300.0 ) -300.0
x1 = x1 /3600.0 -8.0
print 'x.shape', x.shape
print 'x', x
print 'x1.shape', x1.shape
step_size = float(size0-1)/size
print 'step_size', step_size
#for i in range(size):
#  xnew = numpy.arange(0,size0-1,step_size)
# print 'xnew.shape', xnew.shape
# print 'xnew', xnew
#  y = image_data[:,i]
# print 'y.shape', y.shape
#  f = interpolate.interp1d(x, y)
#  new_image[:,i] = f(xnew)   # use interpolation function returned by `interp1d`
#plt.figure()
plt.xlabel('PST time (hours)')
plt.ylabel('Zenith Angle south to north in 5 Deg steps')
plt.title('2017 Aug 22-24 RM Difference (RMextract - ALBUS)')
print 'image_data.shape', image_data.shape
print 'image_data.mean', image_data.mean()
print 'image_data.max', image_data.max()
print 'image_data.min', image_data.min()
#plt.imshow(image_data,extent=(x1[0],x1[x1.shape[0]-1],5,175),aspect="auto",interpolation='none',cmap=plt.cm.seismic,vmin=-15,vmax=15)
#plt.imshow(image_data,extent=(x1[0],x1[x1.shape[0]-1],5,175),aspect="auto",interpolation='none',cmap=plt.cm.seismic,vmin=-0.6,vmax=0.6)
plt.imshow(image_data,extent=(x1[0],x1[x1.shape[0]-1],90-2.5,90-177.5),cmap=plt.cm.jet,aspect="auto",interpolation='none',vmin=-1,vmax=1)
#plt.imshow(image_data)
#for i in range(size0):
#for i in range(6,28):
#  plt.plot(x1,image_data[i,:])
plt.colorbar()
plt.savefig('2017_image_Difference.png')
plt.show()
