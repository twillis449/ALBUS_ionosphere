<!DOCTYPE html>
<html>
<head>
	<meta http-equiv="content-type" content="text/html; charset=utf-8"/>
	<title></title>
	<meta name="generator" content="LibreOffice 7.3.7.2 (Linux)"/>
	<meta name="created" content="00:00:00"/>
	<meta name="changed" content="2023-08-12T16:43:19.562063061"/>
</head>
<body lang="en-CA" dir="ltr">
<div id="page_1" dir="ltr"><p>Please watch the video at
	https://www.youtube.com/watch?v=cO5TYy396xU for detailed
	instructions on how to use the data analysis scripts. Hopefully, it
	will not put you to sleep! More detailed written instructions may
	follow.</p>
	<p>external python packages needed</p>
	<p>numpy, matplotlib, scikit-image, astropy, scipy, shapely , json,
	cosmocalc (at https://cxc.harvard.edu/contrib/cosmocalc/)</p>
	<p>Installation: Put the python code in some directory that's in
	your PYTHONPATH, make the scripts executable, and hopefully things
	will work for you. The code has been tested with python 3.8 on
	Ubuntu 20.04</p>
	<p>There are two main scripts in the package -
	get_morphology_images.py and get_galaxy_parameters.py.</p>
	<p>Get_morphology_images uses morphological erosion and dilation to
	remove background sources from a radio astronomy image. It extends
	the technique described in Rudnick, 2002
	https://iopscience.iop.org/article/10.1086/342499/pdf.</p>
	<p>The process can be described through the following equations:</p>
	<p>o = original image</p>
	<p>d - output from erosion/dilation</p>
	<p>t = white TopHat, which should show only 'compact' structures 
	</p>
	<p>t = o - d</p>
	<p>m = mask derived from a comparison where t &gt; some signal m * t
	= m * (o - d)</p>
	<p>o_d = output diffuse image</p>
	<p>=o - m * t</p>
	<p>=o - (m * o - m * d)</p>
	<p>=o - m * o + (m * d)</p>
	<p>m*d would add the masked dilated image to the 'diffuse' image and
	we do not want to do that so we ignore it to get</p>
	<p>o_d = o - m * o and</p>
	<p>o_c = image of compact objects = m * o</p>
	<p>so the original image equates to o_d + o_c</p>
	<p>We may want to judicious add selected components of o_c to o_d to
	get a final o* We select the components of o_c we wish to add by
	masking their defining polygons to get a mask m_c</p>
	<p>o* = o_d + m_c * o_c</p>
	<p> Now, o_d or o* will still contain  areas with value 0 where we
	subtracted off o_c.  We can fill in the values that we would
	`expect’ to find in these areas by inpainting values from the
	surrounding diffuse emission.  We can do this by either using
	Navier-Stokes inpainting or Fast Marching Method inpainting. (See
	e.g <a href="http://www.ifp.illinois.edu/~yuhuang/inpainting.html">http://www.ifp.illinois.edu/~yuhuang/inpainting.html</a>)
	This will generate a an image o_inp which we can add to  o_d  or o*
	to obtain a ‘filled in’ diffuse image, and subtract from o_c to
	get the ‘actual’ point source signal.</p>
	<p>Get_galaxy_parameters integrates the signal contained within
	specified polygon areas of a radio astronomy image to derive
	integrated flux densities and other parameters of a radio s</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
	<p><br/>
<br/>

	</p>
</div>
</body>
</html>