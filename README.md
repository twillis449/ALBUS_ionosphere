Please watch the video at https://www.youtube.com/watch?v=cO5TYy396xU for detailed instructions on how to use the data analysis scripts. Hopefully, it will not put you to sleep! More detailed written instructions may follow.

external python packages needed
numpy, matplotlib, scikit-image, astropy, scipy, shapely , json,
cosmocalc (at https://cxc.harvard.edu/contrib/cosmocalc/)

Installation: Put the python code in some directory that's in your PYTHONPATH, make the scripts executable, and hopefully things will work for you. The code has been tested with python 3.8 on Ubuntu 20.04 

There are two main scripts in the package - get_morphology_images.py and get_galaxy_parameters.py. Get_morphology_images provides a way to remove background sources from a radio astronomy images. It extends the technique described in Rudnick, 2002 https://iopscience.iop.org/article/10.1086/342499/pdf. Get_galaxy_parameters integrates the signal contained within specified polygon areas of a radio astronomy image to derive integrated flux densities and other parameters of a radio source.
