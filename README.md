<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<HTML>
<HEAD>
<META http-equiv="Content-Type" content="text/html; charset=UTF-8">
<META name="generator" content="BCL easyConverter SDK 5.0.252">
</HEAD>

<BODY>
<DIV id="page_1">


<P class="p0 ft0">Please watch the video at https://www.youtube.com/watch?v=cO5TYy396xU for detailed instructions on how to use the data analysis scripts. Hopefully, it will not put you to sleep! More detailed written instructions may follow.</P>
<P class="p1 ft0">external python packages needed</P>
<P class="p2 ft0">numpy, matplotlib, <NOBR>scikit-image,</NOBR> astropy, scipy, shapely , json, cosmocalc (at https://cxc.harvard.edu/contrib/cosmocalc/)</P>
<P class="p3 ft0">Installation: Put the python code in some directory that's in your PYTHONPATH, make the scripts executable, and hopefully things will work for you. The code has been tested with python 3.8 on Ubuntu 20.04</P>
<P class="p4 ft0">There are two main scripts in the package - get_morphology_images.py and get_galaxy_parameters.py.</P>
<P class="p5 ft0">Get_morphology_images uses morphological erosion and dilation to remove background sources from a radio astronomy image. It extends the technique described in Rudnick, 2002 https://iopscience.iop.org/article/10.1086/342499/pdf.</P>
<P class="p6 ft0">The process can be described through the following equations:</P>
<P class="p6 ft0">o = original image</P>
<P class="p7 ft0">d - output from erosion/dilation</P>
<P class="p8 ft1">t = white TopHat, which should show only compact structures smaller than the structure element</P>
<P class="p7 ft0">t = o - d</P>
<P class="p9 ft1">m = mask derived from a comparison where t &gt; some signal m * t = m * (o - d)</P>
<P class="p7 ft0">o_d = output diffuse image</P>
<P class="p10 ft1"><SPAN class="ft1">=</SPAN><SPAN class="ft2">o - m * t</SPAN></P>
<P class="p10 ft1"><SPAN class="ft1">=</SPAN><SPAN class="ft2">o - (m * o - m * d)</SPAN></P>
<P class="p10 ft0"><SPAN class="ft0">=</SPAN><SPAN class="ft3">o - m * o + (m * d)</SPAN></P>
<P class="p11 ft0">m*d would add the masked dilated image to the 'diffuse' image and we do not want to do that so we ignore it to get</P>
<P class="p12 ft1">o_d = o - m * o and</P>
<P class="p13 ft0">o_c = image of compact objects = m * o</P>
<P class="p14 ft0">so the original image equates to o_d + o_c</P>
<P class="p15 ft0">We may want to judicious add selected components of o_c to o_d to get a final o* We select the components of o_c we wish to add by masking their defining polygons to get a mask m_c</P>
<P class="p14 ft0">o* = o_d + m_c * o_c</P>
<P class="p16 ft0">Get_galaxy_parameters integrates the signal contained within specified polygon areas of a radio astronomy image to derive integrated flux densities and other parameters of a radio source.</P>
</DIV>
</BODY>
</HTML>
