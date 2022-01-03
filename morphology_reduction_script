Basic sequence of operations for morphology erosion and dilation
useing these values:

3 = radius of structure element disk
D = use a 'disk' structure element
6 = make mask = 1 where image signsl (limiting_flux) > 6 times noise
F = true of using convolved image
T = true if processing dilated image

so ...

load original image (get_morphology_images.py abellsouth.csv 3 D 6 F T)

the '3 D' tells the system to make a disk-like 'structure element' with nominal radius 3 pixels:
 [[0 0 0 1 0 0 0]
 [0 1 1 1 1 1 0]
 [0 1 1 1 1 1 0]
 [1 1 1 1 1 1 1]
 [0 1 1 1 1 1 0]
 [0 1 1 1 1 1 0]
 [0 0 0 1 0 0 0]]

experiments showed that 'best results' obtained with two morphology erosion 
operations followed by a dilation

write out dilated image

write out eroded image if using erosion 

if using dilation write out filtered image (original image - dilated image)

if using erosion write out (original image - eroded image)

get noise of original image from breizorro

limiting_flux for mask = noise * limiting_sigma (= 6 used here)

create mask from filtered image, where filtered image signal > limiting flux 
i.e. mask image =  1 where signal of (original image - minus dilated image) 
> limiting flux, and 0 elsewhere

write out mask image

filtered data = mask image * (original image -dilated image) or
filtered data = mask image * (original image -eroded image) 

write out filtered data 

final result = original image - filtered data 
 = original image -  mask image * (original image -filtered_data)

write out final result as a fits file with the ending '_final_minus-filtered_dilated.fits' or ''_final_minus-filtered_eroded.fits'

convolve final result by a factor 2 to smooth out subtraction residuals and get best test for detection of extended emission

If you look at the file ending with '_final_minus-filtered_dilated.fits' you may note that there are still a few locations where sinificant compact signal is left over from the operations described above, so ..


We then copy the file ending with '_final_minus-filtered_dilated.fits' to a new directory, say, named  'second_dilate' and repeat the entire sequence of activities described above  with this file as the starting 'original image'. 
We then end up with a few version of the processed file, again with the ending '_final_minus-filtered_dilated.fits'. If you look at this file you will see that just about all the compact structure in the area of the central diffuse cluster source has disappeared.

final operation - I run a script get_galaxy_parameters.py which runs a pipeline to generate a text file ending with 'source_parameters' that contains things like flux density etc
