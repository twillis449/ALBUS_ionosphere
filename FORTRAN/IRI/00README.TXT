		International Reference Ionosphere (IRI) Software
		-------------------------------------------------

================================================================================
IMPORTANT: Updates and new versions of the IRI programs will be reported through 
the IRInfo electronic mailer distribution. To subscribe send e-mail to 
Majordomo@nssdc.gsfc.nasa.gov with the following text in the body of the
message: subscribe iri (use 'unsubscribe' to remove yourself from the list)
================================================================================

This directory includes the FORTRAN programs and indices of the latest version 
of the International Reference Ionosphere model. The source code consist of 54 
files and a total volume of 3.3 MByte.

The files can be retrieved by anonymous ftp. An on-the-fly compression
utility can be applied to reduce the size of the files and/or to transfer the
contents of this directory as one single file.

To gzip and transfer one file, please use the following command:
		> get filename.gz (e.g. iris13.for.gz)
This will work with the extension '.Z' as well. The tar option can be
used to retrieve a whole subdirectory:
		> get subdir.tar      
or   		> get subdir.tar.gz   for additional compression.
To unzip and untar these files the following UNIX commands should be used:
		> gunzip subdir.tar.gz	    
and then     	> tar -xf subdir.tar

 
Directory Contents:
-------------------

irisub.for	    This file contains the main subroutine iri_sub. It computes 
                height profiles if IRI output parameters (Ne, Te, Ti, Ni, vi) 
                for specified date and location. Also included is the 
                subroutine iri_web that computes output parameters versus
                latitude, longitude (geog. or geom.), year, day of year, hour 
                (LT or UT), and month. An example of how to call and use iri_web 
                is shown in iritest.for. Compilation of iritest.for requires
                irisub.for, irifun.for, iritec.for, iridreg.for, igrf.for, and
                cira.for.

irisubne.for    This is a faster, shorter version of IRI that does not include
                the new D-region and electron temperature models. It is 
                especially recommended for computation of ionospheric electron
		        content. It requires the subroutine files IRIFUN.FOR, 
                IRITECNE.FOR, and CIRA.FOR.

irifun.for	    This file contains the subroutines and functions that are 
                required for running IRI.

iridreg.for     Subroutines for the D region models of Friedrich-Torkar
                and of Danilov et al.

iritec.for 	    This file includes the subroutines for computing the ionospheric 
                electron content from 60km up to a specified upper limit. 

iritecne.for	Special version of iritec.for for use with irine.for

cira.for	    This file includes the subroutines and functions for computing 
		        the COSPAR International Reference Ionosphere 1986 (CIRA-86) 
                neutral temperature. 

igrf.for        This file includes the subroutines for the International
                Geomagnetic Reference Field (IGRF).

dgrf%%.dat      Definitive IGRF coefficients for the years 1945, 1950, .. 1990, 
                and 1995 (%%=45, 50, etc.)(ASCII).
igrf00.dat      IGRF coefficients for 2000 (ASCII).
igrf00s.dat     IGRF coefficients for 2000-2005 (ASCII).

iritest.for 	Test program indicating how to use of the IRI subroutines. 
                Requires irisub, irifun, iritec, iridreg, igrf,and cira.

input.txt       Input parameters for a few examples using the iritest.for program.
output.txt 	    Output from the test program for the input.txt input parameters.

iritestne.for 	Special version of iritest.for for use with irine.for. Requires
                irine, irifun, iritecne, and cira.

inputne.txt     Input parameters for a few examples using the iritestne.for program.
outputne.txt 	Output from the test program for the inputne.txt input parameters.

ig_rz.dat	    This file contains the solar and ionospheric indices (IG12, Rz12) 
                for the time period from Jan 1958 onward. The file is updated 
                quarterly. It is read by subroutine tcon in irifun.for (ASCII). 

ap.dat		    This file provides the 3-hour ap magnetic index from 1960 onward
                (ASCII).

CCIR%%.ASC	    Coefficient files for the global representation of F2 peak
URSI%%.ASC	    peak parameters (foF2, M3000); %% = Month+10 (ASCII). CCIR is
                the model recommended by the International Committee Consultative  
                on Radiocommunication of the International Union of 
                Telecommunication (ITU). URSI is the model recommended by the 
                International Union of Radio Science and by the IRI Working Group.

Please consult the 'listing of changes' in the comments section at the top 
of these programs for recent corrections and improvements.

More information about the IRI project can be found at
	http://nssdc.gsfc.nasa.gov/space/model/ionos/iri.html
IRI parameters can be calculated and plotted online at
	http://nssdc.gsfc.nasa.gov/space/models/iri.html

An IRI Newsletter is published quarterly. To be included on the
distribution or to submit articles of IRI-interest please e-mail
to the Newsletter Editor, K. Oyama, at oyama@bochan.ted.isas.ac.jp

An IRI electronic mailer keeps IRI users informed about model and
software updates. To subscribe please send e-mail to 
Majordomo@nssdc.gsfc.nasa.gov with the following text in the body of
the message: subscribe iri (use 'unsubscribe iri' later if you want
to remove yourself from the distribution).

------------------------ dieter bilitza ------------------ March 2003
