/* pim_asub_c.h */
/* a header file for calling the FORTRAN stuff */
//	2008 Feb 18  JMA  --MPIfR  --rework for gfortran




#ifndef PIM_ASUB_C_H
#define PIM_ASUB_C_H

#ifdef HAVE_G2C_H_FILE
#  include <g2c.h>
#else
#  include <f2c.h>
#endif



#ifdef __cplusplus
extern "C" {
#endif

extern integer pim_asub_c(
    const char* const BASEPATH, /* path to PIM databases */                      
    doublereal STALAT,          /* station latitude in radians */                
    doublereal STALON,          /* station longitude in radians */               
    doublereal STAELEV,         /* station elevation in meters */                
    doublereal OB_AZ,           /* Object azimuth, in radians */                 
    doublereal OB_EL,           /* Object elevation, in radians */               
                                /* All times are UT */                           
    integer TYEAR,              /* Year of observation, as in 2005 */            
    integer TYDAY,              /* Integer day of year, Jan 01 = 1, Feb 01 = 32*/
    integer TMON,               /* Month, Jan=1 */                               
    integer TDAY,               /* Day of month, Jan 01 = 1, Feb 01 = 1 */       
    doublereal TUT,             /* the fractional time of day, in hours */       
    doublereal* STEC,           /* the slant TEC in TECU (10^{16} m^{-2}) */     
    doublereal* SRM,            /* rotation measure in RMU (10^{12} T m^{-2})*/  
    integer* SOLCODE,           /* the Solar flux code, see pim_asub.f */        
    integer* RETCODE            /* the return code, see pim_asub.f */            
    );

extern integer pim_asub_ed_c(
    const char* const BASEPATH, /* path to PIM databases */                      
    doublereal OBSLATD,         /* observation latitude in radians */
    doublereal OBSLOND,         /* observationstation longitude in radians */  
    doublereal OBSELEVD,        /* observationstation elevation in meters */
                                /* All times are UT */                           
    integer TYEAR,              /* Year of observation, as in 2005 */            
    integer TYDAY,              /* Integer day of year, Jan 01 = 1, Feb 01 = 32*/
    integer TMON,               /* Month, Jan=1 */                               
    integer TDAY,               /* Day of month, Jan 01 = 1, Feb 01 = 1 */       
    doublereal TUT,             /* the fractional time of day, in hours */       
    doublereal* E_D,            /* the electron density in m^{-3} */     
    integer* SOLCODE,           /* the Solar flux code, see pim_asub.f */        
    integer* RETCODE            /* the return code, see pim_asub.f */            
    );
#ifdef __cplusplus
}
#endif                          

#endif /* PIM_ASUB_C_H */
