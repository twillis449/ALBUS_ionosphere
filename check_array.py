import numpy as np
# copied from breizorro

# converts input array to be 2-D
def check_array(data):
  if len(data.shape) == 2:
        data = np.array(data[:, :])
  elif len(data.shape) == 3:
        data = np.array(data[0, :, :])
  else:
        data = np.array(data[0, 0, :, :])
  return data

# undoes  the effects of check_array
def update_dimensions(newimage, supplied_dimensions):
# supplied dimensions = hdu.header['NAXIS']
    shape = newimage.shape
    print('shape supplied_dimensions', shape, supplied_dimensions)
    if supplied_dimensions == 2:
        out_image = newimage
    elif supplied_dimensions == 3:
        out_image = np.zeros((1,shape[0], shape[1]), dtype = np.float32)
        out_image[0, :, :] = newimage
    else:
        out_image = np.zeros((1,1,shape[0], shape[1]), dtype = np.float32)
        out_image[0, 0, :, :] = newimage
    return out_image


