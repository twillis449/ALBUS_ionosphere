import numpy as np
# copied from breizorro
def check_array(data):
  if len(data.shape) == 2:
        data = np.array(data[:, :])
  elif len(data.shape) == 3:
        data = np.array(data[0, :, :])
  else:
        data = np.array(data[0, 0, :, :])
  return data

