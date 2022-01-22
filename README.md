Please watch the video at https://www.youtube.com/watch?v=cO5TYy396xU for detailed instructions on how to use the data analysis scripts. Hopefully, it will not put you to sleep! More detailed written instructions may follow.

external python packages needed
numpy, matplotlib, scikit-image, astropy, scipy, shapely , json,
cosmocalc (at https://cxc.harvard.edu/contrib/cosmocalc/)

Installation: Put the python code in some directory that's in your PYTHONPATH, make the scripts executable, and hopefully things will work for you. The code has been tested with python 3.8 on Ubuntu 20.04 

There are two main scripts in the package - get_morphology_images.py and get_galaxy_parameters.py. 

Get_morphology_images uses morphological erosion and dilation to remove background sources from a radio astronomy image. It extends the technique described in Rudnick, 2002 https://iopscience.iop.org/article/10.1086/342499/pdf. 

The process can be described through the following equations:

o = original image \n
d - output from erosion/dilation \n
t = white TopHat, which should show only compact structures smaller than the
    structure element 
t = o - d  
m = mask derived from a comparison where  t > some signal
m * t = m * (o - d)
o_d = output diffuse image
    = o - m * t  
    = o - (m * o - m * d) 
    = o - m * o + (m * d) 

we don't know the flux scale of m * d as we don't know the flux scale of the
dilated image, but it is buried in the output image, so get rid of it
by subtracting it off, which equates to

o_d  = o - m * o
and
o_c = image of compact objects 
    = m * o  

Get_galaxy_parameters integrates the signal contained within specified polygon areas of a radio astronomy image to derive integrated flux densities and other parameters of a radio source.

