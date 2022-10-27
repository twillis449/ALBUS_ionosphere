This software determines the ionosphere total electron content (TEC) over any 
location on the Earth as a function of location and time. It then
uses the TEC and a model of the Earth's magnetc field to compute the 
ionosphere's effect on the Faraday Rotion Meaure (RM) observed for an
astronomical radio source. The ionosphere's contribution to the RM can
then be renoved.  The software may be of interest to both radio astronomers 
and to ionosphere scientists. Test observations suggest that our analysis 
gives results consistent with those found by on-site site experiments that 
used local GPS receivers. 

The software derives the TEC of the ionosphere by using publicly available 
observation data of Global Positioning System (GPS) satellites. It
searches through a database of several thousand ground-based GPS receivers and
then gets GPS receiver data from those stations located within a specified 
distance of the position of the telescope being used for the radio 
astronomy observation.

In principle, the Faraday rotation angle is a function of source
direction and antenna position, but Faraday rotation is usually 
a large-scale effect and it may have approximately the same value across 
an entire telescope primary beam field of view (perhaps about one degree). 
For arrays smaller than a few kilometres, the rotation angle will usually
also be the same for all stations. These assumptions reduce the number of 
independent parameters considerably, but they may  break down as the observing 
 wavelength gets longer due to the wavelength squared effect and increasing 
field of view, as well as when telescope arrays have longer baselines.

In reality, from the ground we usually cannot directly measure the 
distribution of the electrons along the line of sight nor directly measure 
the magnetic field strength as a function of position and direction. 
In order to calculate a rotation measure, many routines place all the electrons
at some "standard" height and attach a magnetic field value from a model of the
terrestrial field. In contrast, the software that we describe goes beyond
this simple algorithm by distributing the electrons along the line of sight
taking into account modern understanding of ionospheric physics, and employs a
model of the terrestrial magnetic field that accounts for change of intensity
and direction with height.

The software makes extensive use of python scripts and should work with both
python2 and python3.

You need to install pycurl, astropy, pyephem or python casacore, numpy 
and matplotlib for the system to work. A number of support programs to handle 
RINEX files are also needed. These programs are specified in the INSTALL 
file.

More sites (especially Geosciences Australia) are now producing only RINEX3 files. 
For analysis, we use RINEX2 files. To convert RINEX3 to RINEX2 you need to get 
and install gfzrnx, available from https://gnss.gfz-potsdam.de/services/gfzrnx
and RX3name (see http://acc.igs.org/software.html) Unfortunately these programs 
seem to be available only in binary format.

Unfortunately, due to security concerns, sites are also changing access methods 
from simple anonymous ftp to more secure procedures such as sftp etc and we are
currently working on modifying our access procedures to reach such sites.
secure access procedures 

A somewhat more detained description of the software is given in 
the, as yet, unpublished paper twillis_ALBUS_paper.pdf available in 
this directory.
