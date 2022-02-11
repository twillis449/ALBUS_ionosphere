<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
<head>
	<meta http-equiv="content-type" content="text/html; charset=utf-8"/>
	<title></title>
	<meta name="generator" content="LibreOffice 6.4.7.2 (Linux)"/>
	<meta name="created" content="00:00:00"/>
	<meta name="changed" content="2022-02-11T11:10:43.681202912"/>
	<style type="text/css">
		@page { size: 21.59cm 27.94cm; margin: 2cm }
		p { margin-bottom: 0.25cm; line-height: 115%; background: transparent }
		pre { background: transparent }
		pre.western { font-family: "Liberation Mono", monospace; font-size: 10pt }
		pre.cjk { font-family: "Noto Sans Mono CJK SC", monospace; font-size: 10pt }
		pre.ctl { font-family: "Liberation Mono", monospace; font-size: 10pt }
	</style>
</head>
<body lang="en-CA" link="#000080" vlink="#800000" dir="ltr"><pre class="western">Please watch the video at https://www.youtube.com/watch?v=cO5TYy396xU for detailed instructions on how to use the data analysis scripts. Hopefully, it will not put you to sleep! More detailed written instructions may follow.

external python packages needed
numpy, matplotlib, scikit-image, astropy, scipy, shapely , json,
cosmocalc (at https://cxc.harvard.edu/contrib/cosmocalc/)

Installation: Put the python code in some directory that's in your PYTHONPATH, make the scripts executable, and hopefully things will work for you. The code has been tested with python 3.8 on Ubuntu 20.04

There are two main scripts in the package - get_morphology_images.py and get_galaxy_parameters.py.

Get_morphology_images uses morphological erosion and dilation to remove background sources from a radio astronomy image. It extends the technique described in Rudnick, 2002 https://iopscience.iop.org/article/10.1086/342499/pdf.

The process can be described through the following equations:

o = original image
d - output from erosion/dilation
t = white TopHat, which should show only compact structures smaller than the
    structure element
t = o - d  
m = mask derived from a comparison where  t &gt; some signal
m * t = m * (o - d)
o_d = output diffuse image
    = o - m * t  
    = o - (m * o - m * d)
    = o - m * o + (m * d)

m*d would add the masked dilated image to the 'diffuse' image and we do not want to do that so we ignore it to get
o_d  = o - m * o
and
o_c = image of compact objects
    = m * o  

so the original image equates to o_d + o_c

We may want to judicious add selected components of o_c to o_d to get a final o* 
We select the components of o_c we wish to add by masking their defining
polygons to get a mask m_c

o* = o_d + m_c * o_c

Get_galaxy_parameters integrates the signal contained within specified polygon areas of a radio astronomy image to derive integrated flux densities and other parameters of a radio source.</pre>
</body>
</html>