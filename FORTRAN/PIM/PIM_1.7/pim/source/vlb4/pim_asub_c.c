/* pim_asub_c.c */
/* a file for calling the FORTRAN stuff */
//	2008 Feb 18  JMA  --MPIfR  --rework for gfortran



#ifdef HAVE_G2C_H_FILE
#  include <g2c.h>
#else
#  include <f2c.h>
#endif
#include "string.h"
#include <stdio.h>
#include "JMA_code.h"

#include "pim_asub_c.h"



/* this is the FORTRAN function */
extern integer FTN_NAME(pim_asub)(
    char* path, 
    doublereal* STALAT,
    doublereal* STALON,
    doublereal* STAELEV,
    doublereal* OB_AZ,
    doublereal* OB_EL,
    integer* TYEAR,
    integer* TYDAY,
    integer* TMON,
    integer* TDAY,
    doublereal* TUT,
    doublereal* STEC,
    doublereal* SRM,
    integer* SOLCODE,
    integer* RETCODE,
    integer
    );
/* This is the electron density FORTRAN function */
extern integer FTN_NAME(pim_asub_ed)(
    char* path, 
    doublereal* OBSLATD,
    doublereal* OBSLOND,
    doublereal* OBSELEVD,
    integer* TYEAR,
    integer* TYDAY,
    integer* TMON,
    integer* TDAY,
    doublereal* TUT,
    doublereal* E_D,
    integer* SOLCODE,
    integer* RETCODE,
    integer
    );






integer pim_asub_c(
    const char* const BASEPATH,
    doublereal STALAT,
    doublereal STALON,
    doublereal STAELEV,
    doublereal OB_AZ,
    doublereal OB_EL,
    integer TYEAR,
    integer TYDAY,
    integer TMON,
    integer TDAY,
    doublereal TUT,
    doublereal* STEC,
    doublereal* SRM,
    integer* SOLCODE,
    integer* RETCODE
    )
{
    const integer SIZE = 256;
    char path[SIZE];
    int i;

    /* convert elevation from meters to km */
    STAELEV *= 0.001;
    
    strncpy(path, BASEPATH, SIZE);
    for(i=strlen(path); i < SIZE; i++) path[i] = ' ';
    FTN_NAME(pim_asub)(path,&STALAT,&STALON, &STAELEV,
                       &OB_AZ, &OB_EL,
                       &TYEAR, &TYDAY, &TMON, &TDAY, &TUT,
                       STEC, SRM, SOLCODE, RETCODE,SIZE);
    return 0;
}



integer pim_asub_ed_c(
    const char* const BASEPATH,
    doublereal OBSLATD,
    doublereal OBSLOND,
    doublereal OBSELEVD,
    integer TYEAR,
    integer TYDAY,
    integer TMON,
    integer TDAY,
    doublereal TUT,
    doublereal* E_D,
    integer* SOLCODE,
    integer* RETCODE
    )
{
    const integer SIZE = 256;
    char path[SIZE];
    int i;

    /* convert elevation from meters to km */
    OBSELEVD *= 0.001;
    
    strncpy(path, BASEPATH, SIZE);
    for(i=strlen(path); i < SIZE; i++) path[i] = ' ';
    FTN_NAME(pim_asub_ed)(path,&OBSLATD,&OBSLOND, &OBSELEVD,
                       &TYEAR, &TYDAY, &TMON, &TDAY, &TUT,
                       E_D,SOLCODE, RETCODE,SIZE);
    return 0;
}
