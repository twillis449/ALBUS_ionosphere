#include <stdio.h>

#include "../vex.h"
#include "../y.tab.h"

#define YES
/* globals */

extern int yydebug;
extern int lines;

main(int argc, char *argv[])
{
  void *ptr;
  Llist *lowls,*lowls2;
  struct vex *vex;
  char *cptr;

  if (argc > 2)
    yydebug=1;
  else
    yydebug=0;

  if(vex_open(argv[1],&vex))
    fprintf(stderr,"error opening and parsing vex file %s\n",argv[1]);
#ifdef YES
  print_vex(vex);
  return;
#endif
  
  lowls=get_scan(&cptr,vex);

  while(lowls != NULL) {
    printf("\n scanid %s",cptr);

    ptr=get_scan_mode(lowls);
    print_lowl_st(T_MODE,ptr);
    ptr=get_scan_source(lowls);
    if(ptr==NULL)
      printf(" source not found\n");
    else {
      print_lowl_st(T_SOURCE,ptr);
      while(NULL!=(ptr=get_scan_source_next())){
	print_lowl_st(T_SOURCE,ptr);
      }
    }    
    lowls2=get_data_transfer_scan(lowls);
    while(lowls2!=NULL) {
      print_lowl_st(T_DATA_TRANSFER,lowls2);
      lowls2=get_data_transfer_scan_next();
    }

    lowls=get_scan_next(&cptr);
  }
  
  return;
#ifdef NO
  while(lowls != NULL) {
    printf("\n scanid %s",cptr);

    ptr=get_scan_mode(lowls);
    print_lowl_st(T_MODE,ptr);
    ptr=get_scan_source(lowls);
    if(ptr==NULL)
      printf(" source not found\n");
    else {
      print_lowl_st(T_SOURCE,ptr);
      while(NULL!=(ptr=get_scan_source_next())){
	print_lowl_st(T_SOURCE,ptr);
      }
    }    
    lowls2=get_station_scan(lowls);
    while(lowls2!=NULL) {
      print_lowl_st(T_STATION,lowls2);
      lowls2=get_station_scan_next();
    }

    lowls=get_scan_next(&cptr);
  }
  
  return;

  ptr=get_all_lowl("EF","SX_VLBA",T_CHAN_DEF,B_FREQ,vex);
  if(ptr==NULL)
    printf("not found\n");
  else
    print_lowl_st(T_CHAN_DEF,ptr);

  while(NULL!=(ptr=get_all_lowl_next())){
    print_lowl_st(T_CHAN_DEF,ptr);
  }
  if(ptr==NULL)
    printf("\nnot found\n");

  ptr=get_all_lowl("JB",NULL,T_RECORD_TRANSPORT,B_DAS,vex);
  if(ptr==NULL)
    printf("not found\n");
  else
    print_lowl_st(T_RECORD_TRANSPORT,ptr);

  while(NULL!=(ptr=get_all_lowl_next())){
    print_lowl_st(T_RECORD_TRANSPORT,ptr);
    printf("\n");
  }
  if(ptr==NULL)
    printf("\nnot found\n");

  ptr=get_all_lowl(NULL,NULL,T_X_WOBBLE,B_EOP,vex);
  if(ptr==NULL)
    printf("not found\n");
  else
    print_lowl_st(T_X_WOBBLE,ptr);

  while(NULL!=(ptr=get_all_lowl_next())){
    print_lowl_st(T_X_WOBBLE,ptr);
    printf("\n");
  }
  if(ptr==NULL)
    printf("\nnot found\n");

  ptr=get_source_lowl("HD123456",T_RA,vex);
  if(ptr==NULL)
    printf("not found\n");
  else
    print_lowl_st(T_RA,ptr);

  while(NULL!=(ptr=get_source_lowl_next())){
    print_lowl_st(T_RA,ptr);
    printf("\n");
  }
  if(ptr==NULL)
    printf("\nnot found\n");

  ptr=get_station_def(vex);
  if(ptr==NULL)
    printf("not found\n");
  else {
    print_svalue(ptr);
    printf("\n");
  }
  while(NULL!=(ptr=get_station_def_next())){
    print_svalue(ptr);
    printf("\n");
  }
  if(ptr==NULL)
    printf("\nnot found\n");

  ptr=get_mode_def(vex);
  if(ptr==NULL)
    printf("not found\n");
  else {
    print_svalue(ptr);
    printf("\n");
  }
  while(NULL!=(ptr=get_mode_def_next())){
    print_svalue(ptr);
    printf("\n");
  }
  if(ptr==NULL)
    printf("\nnot found\n");

  ptr=get_source_def(vex);
  if(ptr==NULL)
    printf("not found\n");
  else {
    print_svalue(ptr);
    printf("\n");
  }
  while(NULL!=(ptr=get_source_def_next())){
    print_svalue(ptr);
    printf("\n");
  }
  if(ptr==NULL)
    printf("\nnot found\n");

  ptr=get_scan_station(&lowls,"JB",vex);
  if(ptr==NULL)
    printf("not found\n");
  else {
    print_lowl_st(T_STATION,ptr);
    printf("\n");
    ptr=get_scan_mode(lowls);
    print_lowl_st(T_MODE,ptr);
    printf("\n");
    ptr=get_scan_source(lowls);
    if(ptr==NULL)
      printf(" source not found\n");
    else {
      print_lowl_st(T_SOURCE,ptr);
      printf("\n");
      while(NULL!=(ptr=get_scan_source_next())){
	print_lowl_st(T_SOURCE,ptr);
	printf("\n");
      }
      if(ptr==NULL)
	printf("\n source not found\n");
    }    
  }
  while(NULL!=(ptr=get_scan_station_next(&lowls))){
    print_lowl_st(T_STATION,ptr);
    printf("\n");
    ptr=get_scan_mode(lowls);
    print_lowl_st(T_MODE,ptr);
    printf("\n");
    ptr=get_scan_source(lowls);
    if(ptr==NULL)
      printf(" source not found\n");
    else {
      print_lowl_st(T_SOURCE,ptr);
      printf("\n");
      while(NULL!=(ptr=get_scan_source_next())){
	print_lowl_st(T_SOURCE,ptr);
	printf("\n");
      }
      if(ptr==NULL)
	printf("\n source not found\n");
    }    
  }
  if(ptr==NULL)
    printf("\nnot found\n");
#endif
}


