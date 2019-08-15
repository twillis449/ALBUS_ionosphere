      program t
      implicit none
      character*129 buffer,mode,units
      integer ierr,vex,lenn,i, iarray(4),int
      double precision double, seconds
c
      integer ptr_ch,fvex_open,fvex_len,fvex_field,fvex_units
      integer fget_station_def,fget_mode_def,fget_source_def
      integer fget_all_lowl,fget_mode_lowl,fget_station_lowl
      integer fget_global_lowl,fget_source_lowl,fget_scan_station
      integer fvex_scan_source
      integer fvex_double,fvex_date,fvex_int,fvex_ra,fvex_dec
c
      ierr=fvex_open(ptr_ch("vex13.skd"//char(0)),vex)

      write(6,*) "ierr from fvex_open=",ierr," vex=",vex

      ierr=fget_station_def(ptr_ch(buffer),len(buffer),vex)
      write(6,*) "ierr from fget_station_def=",ierr
c
      if(ierr.eq.0) then
         lenn=fvex_len(buffer)
         write(6,*)"fvex_len(buffer)=",lenn
      endif
c
      do while (lenn.gt.0.and.ierr.eq.0)
         write(6,*) "buffer=",buffer(1:lenn)
         ierr=fget_station_def(ptr_ch(buffer),len(buffer),0)
         write(6,*) "ierr from fget_station_def=",ierr
         if(ierr.eq.0) then
            lenn=fvex_len(buffer)
            write(6,*)"fvex_len(buffer)=",lenn
         endif
      enddo
c
      ierr=fget_mode_def(ptr_ch(buffer),len(buffer),vex)
      write(6,*) "ierr from fget_mode_def=",ierr
c
      if(ierr.eq.0) then
         lenn=fvex_len(buffer)
         write(6,*)"fvex_len(buffer)=",lenn
      endif
c
      do while (lenn.gt.0.and.ierr.eq.0)
         write(6,*) "buffer=",buffer(1:lenn)
         ierr=fget_mode_def(ptr_ch(buffer),len(buffer),0)
         write(6,*) "ierr from fget_mode_def=",ierr
         if(ierr.eq.0) then
            lenn=fvex_len(buffer)
            write(6,*)"fvex_len(buffer)=",lenn
         endif
      enddo
c
      ierr=fget_source_def(ptr_ch(buffer),len(buffer),vex)
      write(6,*) "ierr from fget_source_def=",ierr
c
      if(ierr.eq.0) then
         lenn=fvex_len(buffer)
         write(6,*)"fvex_len(buffer)=",lenn
      endif
c
      do while (lenn.gt.0.and.ierr.eq.0)
         write(6,*) "buffer=",buffer(1:lenn)
         ierr=fget_source_def(ptr_ch(buffer),len(buffer),0)
         write(6,*) "ierr from fget_source_def=",ierr
         if(ierr.eq.0) then
            lenn=fvex_len(buffer)
            write(6,*)"fvex_len(buffer)=",lenn
         endif
      enddo
c
      ierr=fget_all_lowl(ptr_ch("EF"//char(0)),ptr_ch("SX"//char(0)),
     &     ptr_ch("chan_def"//char(0)),ptr_ch("FREQ"//char(0)),vex)
      write(6,*) "ierr from fget_all_lowl=",ierr

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif
      enddo
c
      ierr=fget_mode_lowl(ptr_ch("JB"//char(0)),
     &     ptr_ch("SX_VLBA"//char(0)),
     &     ptr_ch("chan_def"//char(0)),ptr_ch("FREQ"//char(0)),vex)
      write(6,*) "ierr from fget_mode_lowl=",ierr

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif
      enddo
C
      ierr=fget_station_lowl(ptr_ch("EF"//char(0)),
     &     ptr_ch("site_position"//char(0)),ptr_ch("SITE"//char(0)),vex)
      write(6,*) "ierr from fget_station_lowl=",ierr

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif

      enddo
      ierr=fget_station_lowl(ptr_ch("EF"//char(0)),
     &     ptr_ch("ant_motion"//char(0)),ptr_ch("ANTENNA"//char(0)),vex)
      write(6,*) "ierr from fget_station_lowl=",ierr

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif

      enddo
C
      ierr=fget_global_lowl(
     &     ptr_ch("x_wobble"//char(0)),ptr_ch("EOP"//char(0)),vex)
      write(6,*) "ierr from fget_global_lowl=",ierr

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif

      enddo
c
      ierr=fget_source_lowl(ptr_ch("HD123456"//char(0)),
     & ptr_ch("source_model"//char(0)),vex)
      write(6,*) "ierr from fget_source_lowl=",ierr

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif

      enddo
c
      ierr=fget_source_lowl(ptr_ch("HD123456"//char(0)),
     & ptr_ch("source_model"//char(0)),0)
      write(6,*) "ierr from fget_source_lowl=",ierr

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif

      enddo
c
      ierr=fget_scan_station(ptr_ch(buffer),len(buffer),
     &     ptr_ch(mode),len(mode),ptr_ch("JB"//char(0)),vex)
      write(6,*) "ierr from fget_scan_station=",ierr
      write(6,*) "start=",buffer(1:fvex_len(buffer))
      write(6,*) "mode =",mode(1:fvex_len(mode))

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif

      enddo
c
      ierr=fget_scan_station(ptr_ch(buffer),len(buffer),
     &     ptr_ch(mode),len(mode),ptr_ch("JB"//char(0)),0)
      write(6,*) "ierr from fget_scan_station=",ierr
      write(6,*) "start=",buffer(1:fvex_len(buffer))
      write(6,*) "mode =",mode(1:fvex_len(mode))

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif
      enddo
c
      ierr=fget_scan_station(ptr_ch(buffer),len(buffer),
     &     ptr_ch(mode),len(mode),ptr_ch("JB"//char(0)),0)
      write(6,*) "ierr from fget_scan_station=",ierr
      write(6,*) "start=",buffer(1:fvex_len(buffer))
      write(6,*) "mode =",mode(1:fvex_len(mode))

      ierr=fvex_date(ptr_ch(buffer),iarray,seconds)
      write(6,*) "ierr from fvex_date=",ierr
      write(6,*) "iarray = ", iarray, " seconds=",seconds

      do i=1,9
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

      ierr=fvex_units(ptr_ch(units),len(units))
      write(6,*) "i=",i," ierr from fvex_units=",ierr

      if(fvex_len(units).gt.0) then
         write(6,*) "units='",units(1:fvex_len(units)),
     &        "' len=",fvex_len(units)
         ierr=fvex_double(ptr_ch(buffer),ptr_ch(units),double)
         write(6,*) " ierr from fvex_double=",ierr," doube=",double
      endif

      ierr=fvex_int(ptr_ch(buffer),int)
      write(6,*) "ierr from fvex_int=",ierr
      write(6,*) "int= ",int

      enddo

      do i=1,3
      ierr=fvex_scan_source(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_scan_source=",ierr

      if(fvex_len(buffer).gt.0)
     &write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)
      enddo
c
      ierr=fget_source_lowl(ptr_ch("HD123456"//char(0)),
     & ptr_ch("ra"//char(0)),vex)
      write(6,*) "ierr from fget_source_lowl=",ierr

      i=1
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0) then
      write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

         ierr=fvex_ra(ptr_ch(buffer),double)
         write(6,*) " ierr from fvex_ra=",ierr," doube=",double
      endif
c
      ierr=fget_source_lowl(ptr_ch("HD123456"//char(0)),
     & ptr_ch("dec"//char(0)),vex)
      write(6,*) "ierr from fget_source_lowl=",ierr

      i=1
      ierr=fvex_field(i,ptr_ch(buffer),len(buffer))
      write(6,*) "i=",i," ierr from fvex_field=",ierr

      if(fvex_len(buffer).gt.0) then
      write(6,*) "buffer='",buffer(1:fvex_len(buffer)),
     & "' len=",fvex_len(buffer)

         ierr=fvex_dec(ptr_ch(buffer),double)
         write(6,*) " ierr from fvex_dec=",ierr," doube=",double
      endif
c
      end
