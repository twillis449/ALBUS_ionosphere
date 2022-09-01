# equations taken from George Miley 1980 Review Article in Annual Reviews
# we use a mean angle of 40 deg between the uniform magnetic field and
# the line of sight
# we use a spectral index of -0.8
# low freq = 30 MHz, high freq = 10 GHz

# 1 pc= 3.085678e+16 m

import math
from cosmocalc import cosmocalc

Ho = 70             # Hubble Constant
Omega_w = 0.3

c =  299792458.0    # speed of light in metres per second
c = c / 1000.00     # km / sec

def ang_path(z, angle):
#z = redshift
#angle = angle we wish to guess distance through the source in arcsec 
#  print('input z', z, Ho, Omega_w)
# print('processing angle', angle)
  result = cosmocalc(z, H0=Ho, WM=Omega_w)
  LAS_dist = result['DA_Mpc'] *1000.0
  LUM_dist = result['DL_Mpc'] *1000.0
# print('LAS distance kpc)', LAS_dist)
# print('LUM distance kpc)', LUM_dist)
# print('distance ratio LUM_dist /LAS_dist', LUM_dist /LAS_dist)
  angle = math.radians(angle / 3600.0)
  ang_p = LAS_dist * angle
  return ang_p, LAS_dist, LUM_dist

def equipartition_accurate(freq,theta_1, theta_2, theta_actual, flux, n_flux,z, alpha):
   LAP, LAS_dist, LUM_dist = ang_path(z, theta_2) 
   print('equipartition fluxes: total, nuclear ', flux, n_flux)
#  print ('LAP in kpc ', LAP)
   angle = math.radians(theta_actual/ 3600.0)
   source_apparent_size = LAS_dist * angle
#  print('source apparent size (kpc) ', source_apparent_size)
   a = 1.0 + z
   lobe_flux = flux - n_flux 
   print('lobe flux is ', lobe_flux)
   flux_ratio = lobe_flux/ (theta_1 * theta_2 * LAP)
#  print('flux_ratio', flux_ratio)
   gain = 3.0 - alpha
   b = 2.0 * math.pow(a, gain) * flux_ratio  
   mu1 = 0.01
   mu2 = 100
   mu0 =  freq
   c = math.pow(mu2, alpha+0.5) - math.pow(mu1, alpha+0.5)
   d = math.pow(mu0, alpha) * (alpha + 0.5)
   try:
      B_me_1 = 5.69e-5 * math.pow(b*c/d, 2.0/7.0)
   except:
      print('failure to calculate B_me_1')
      print('b',b)
      print('c',c)
      print('d',d)
      B_me_1 = 0.0
   B_me = 1.0e-4 * math.pow(a,1.1) *  math.pow(freq,0.22)  * math.pow(flux_ratio, (2.0/7.0))

#  print('**** mag fields ', B_me, B_me_1 )
   u_me = 1.9e-9 * math.pow(a,2.2) *  math.pow(freq,0.44)  * math.pow(flux_ratio, (4.0/7.0))
#  print('min energy', u_me)
   u_me1 = (7.0 / 3.0) * (B_me_1 * B_me_1) / (math.pi * 8.0)
#  print('min energy 1', u_me1)
#  print('ratio', u_me1 / u_me)
   pressure = u_me / 0.3
   B_me_equiv = math.sqrt ((math.pi * 8.0) * (3.0 / 7.0) * u_me)
#  print('mag field equiv', B_me_equiv)
 
# OK - equipartition parameters calculated - now lets get k-corrected radio luminosity at 1.4 GHz
   S_0_9 = 1.0e-26 * flux    # add in 1e-26 conversion factor to convert Jy to units / m^2
   S_1_4 = S_0_9 * math.pow(1.4/freq, alpha)     
# radio K correction = (1+z)^((-1.0*alpha) - 1) so (-1.0*alpha) - 1) -0.2 for alpha = -0.8
# see https://academic.oup.com/mnras/article/392/2/617/976712
   k_corr = -1.0 * (-1.0*alpha - 1)
   e = 1.0 / math.pow(a, 0.2)                # -0.8 + 1
   e = 1.0 / math.pow(a, k_corr)             # -0.8 + 1
#  print ('k_corr', e)
   LUM_dist_m = LUM_dist *1000.0 * 3.085678e+16    # convert lum distance from KPc to metres
   lum_1_4 = 4.0 * math.pi * math.pow(LUM_dist_m, 2.0) * S_1_4  * e
   lum_0_9 = 4.0 * math.pi * math.pow(LUM_dist_m, 2.0) * S_0_9  * e
   out_str = 'calculated ' + str(freq) + ' GHz luminosity'
#  print(out_str, lum_0_9)
#  print('calculated 1.4 GHz luminosity', lum_1_4)
#  volume should be in kpc^3 converted to cm^3
#  1pc= 3.085678e+18
#  so 1 kpc = 3.085678e+21 cm
   one_kpc_cubed = math.pow(3.085678e+21,3)
   volume = math.pi * (4.0/3.0) * (theta_1/theta_2) * math.pow(0.5*LAP,3) *one_kpc_cubed
   return B_me, u_me, LAS_dist, LAP, LUM_dist, (lum_0_9, lum_1_4), source_apparent_size, volume
