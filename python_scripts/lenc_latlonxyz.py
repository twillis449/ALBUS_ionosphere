#!/usr/bin/env python

import math
import ephem

sm_a = 6378137.0
invf = 298.257223563
f = 1.0 / invf

# Convert geodetic (WGS84) latitude (radians), longitude (radians) 
# and elevation (metres) to ITRF XYZ
def WGS84ToITRF(lat, lon, h): # WGS-84 to ITRF
   SINK = math.sin(lat)
   COSK = math.cos(lat)
   e2 = 2.0 * f - f * f
   v = sm_a / math.sqrt(1.0 - e2 * SINK * SINK)
   x = (v + h) * COSK * math.cos(lon)
   y = (v + h) * COSK * math.sin(lon)
   z = ((1 - e2) * v + h) * SINK
   return x, y, z

# Convert ITRF to WGS84
def ITRFToWGS84(x, y, z):
   e2 = 2.0 * f - f * f
   E = e2 / (1.0 - e2)
   b = sm_a * (1.0 - f)
   p = math.sqrt(x * x + y * y)
   q = math.atan2(z * sm_a, (p * b))
   lat = math.atan2((z + E * b * math.sin(q) * math.sin(q) * math.sin(q)), (p - e2 * sm_a * math.cos(q) * math.cos(q) * math.cos(q)))
   v = sm_a / math.sqrt(1.0 - e2 * math.sin(lat) * math.sin(lat))
   lon = math.atan2(y, x)
   h = (p / math.cos(lat)) - v
   lat = math.degrees(lat)
   lon = math.degrees(lon)
# make sure longitude in range 0 -> 360 degrees
   if lon < 0:
     lon = lon + 360.0
   return lat, lon, h
	
########################################

lat = ephem.degrees("-26:42:11.95")
lon = ephem.degrees("116:40:14.93")
el = 377.83

# MWA MRO1 GPS position
lon = ephem.degrees("116:38:15.0")
lat = ephem.degrees("-26:41:47.9")
el = 354.1

x, y, z = WGS84ToITRF(lat, lon, el)
print("MRO1 GPS X=%f, Y=%f, Z=%f" %(x, y, z))
lat2, lon2, el2 = ITRFToWGS84(x, y, z)
print("lat=%f, lon=%f, el=%f" %(lat2, lon2, el2))
print(' ')
print('===================')

# Parkes, Australia
lon = ephem.degrees("148:15:52.6")
lat = ephem.degrees("-32:59:55.6")
el = 397.4

x, y, z = WGS84ToITRF(lat, lon, el)
print("Parkes GPS X=%f, Y=%f, Z=%f" %(x, y, z))
lat2, lon2, el2 = ITRFToWGS84(x, y, z)
print("lat=%f, lon=%f, el=%f" %(lat2, lon2, el2))
print(' ')
print('===================')
# VLIS APPROX POSITION XYZ
x=3975805.2330  
y=249950.0838  
z=4964446.0974 
lat2, lon2, el2 = ITRFToWGS84(x, y, z)
print("VLIS lat=%f, lon=%f, el=%f" %(lat2, lon2, el2))
# IJMU   APPROX POSITION XYZ 
x=3882053.2948   
y=309346.2124  
z=5034330.2513 
lat2, lon2, el2 = ITRFToWGS84(x, y, z)
print("IJMU lat=%f, lon=%f, el=%f" %(lat2, lon2, el2))
# VLIE  APPROX POSITION XYZ
x=3805256.1464  
y=339064.9313  
z=5090357.9056 
lat2, lon2, el2 = ITRFToWGS84(x, y, z)
print("VLIE lat=%f, lon=%f, el=%f" %(lat2, lon2, el2))
# MWA  APPROX POSITION XYZ
x=-2559129.58799
y= 5095564.16506
z=-2848999.9728
lat2, lon2, el2 = ITRFToWGS84(x, y, z)
print("MWA lat=%f, lon=%f, el=%f" %(lat2, lon2, el2))

#DRAO GPS
x = -2059164.9628  
y = -3621108.4100  
z = 4814432.2660
x = -2.059006522873E+06
y = -3.621245746518E+06 
z = 4.814389426402E+06 
lat2, lon2, el2 = ITRFToWGS84(x, y, z)
print("DRAO lat=%f, lon=%f, el=%f" %(lat2, lon2, el2))

# ASKAP
x= -2556630.504912
y = 5097137.856480 
z =-2848385.218557
lat2, lon2, el2 = ITRFToWGS84(x, y, z)
print("ASKAP lat=%f, lon=%f, el=%f" %(lat2, lon2, el2))
