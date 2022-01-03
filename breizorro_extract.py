import scipy.special
import scipy.ndimage
import numpy as np

# extracted from breizorro (https://github.com/ratt-ru/breizorro)

def make_noise_map(input_image_array, boxsize=50):
    # Cyril's magic minimum filter
    # Plundered from the depths of https://github.com/cyriltasse/DDFacet/blob/master/SkyModel/MakeMask.py

    box = (boxsize, boxsize)
    n = boxsize**2.0
    x = np.linspace(-10, 10, 1000)
    f = 0.5 * (1.0 + scipy.special.erf(x / np.sqrt(2.0)))
    F = 1.0 - (1.0 - f)**n
    ratio = np.abs(np.interp(0.5, F, x))
    noise = -scipy.ndimage.filters.minimum_filter(input_image_array, box) / ratio
    negative_mask = noise < 0.0
    noise[negative_mask] = 1.0e-10
    median_noise = np.median(noise)
    median_mask = noise < median_noise
    noise[median_mask] = median_noise
    return median_noise
