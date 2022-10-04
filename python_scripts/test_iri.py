
from MS_Iono_functions import * 

# The following function should be the only one which needs changes that depend
# on the type of observation that one is doing

def process_ionosphere(MSname="",MSdir=".", Ra=0, Dec=0, Az=180.0, El=-90.0, Lat=0, Long=0, Height=0,start_time="",end_time="", object="",time_step=300.0,telescope_pos=None,station_pos_x=0,station_pos_y=0,station_pos_z=0,processing_option="MO_PIM",tolerance=0.1,overwrite=0,do_serial=0,raise_bias_error=1, num_processors=4,use_pim=1,use_global_data=0,gps_data_directory="."):

# Inputs:
# MSname - name of Measurement Set (MS) to use, if you want to get RM and TEC for a specific MS
# MSdir  - name of directory where MS is stored. Default = current
# Ra     - Right Ascension of field centre. Default = get it from MS, otherwise specify if you want to track a specific direction
# Dec    - Declination of field centre. Default = get it from MS, otherwise specify if you want to track a specific direction
# Az     - azimuth, north through east. Default = derive from Ra and Dec for tracking interferometer 
# El     - elevation, Default = derive from Ra and Dec for tracking interferometer 
# station_pos - XYZ location of antenna. Default = get mean position of array from MS
# processing_option - type of solution. Default = MO_PIM
# tolerance - not sure what changing this will actually do. Default = 0.1
# overwrite - download new data if previous data exists. Default = do not dowmload
# do_serial - get GPS data sequentially. Default -  get GPS data in parallel
# num_processors - number of sub-tasks to run when getting data in parallel. 
# Default = 2, which is OK for core2duo machine. Also see comments below for 
# Geosciences Australia
# use_pim - 1 Use PIM model (default) in fit, 0: Use IRI model
# use_global_data - 1: include data from global GNSS network, 0: ignore data from global network
# gps_data_directory - directory where the GPS data is to be stored. Default = current one.

# Lat - latitude of the observatory
# Long - longitude of the observatory
# Height - height of the observatory
# If latitude and longitude are specified, then these locations will be used 
# find the location of the observing site
#
# start_time - start date and time of the observation (string)
# end_time - enddate and time of the observation (string)
# if these two parameters are specified then the dates are not obtained from a
# measurement set
# time_step - seconds to increment in each cycle of ionosphere prediction
# object - name of what is being observed - can be left blank 
# raise_bias_error - if set to 1, will ignore all GPS observations where bias 
#   has not been corrected for. For Western Australia you meed to set this
#   parameter to 0 as outherwise you will only get a few of  the 22 stations
#   that are available.

    os.system('date')
    process_start = time.time()
    startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print( "process_ionosphere Start at %s" % startime)

    if len(MSname) == 0:
        print( '*************')
        print( ' No measurement set specified - assuming that specifications')
        print( ' can be obtained from command line')
        print( '*************')

    if do_serial:
      if len(MSname) > 0:
        out_file = 'albus_report_serial_' + processing_option + '_'+ MSname
      else:
        out_file = 'albus_report_serial_' + processing_option
    else:
      if len(MSname) > 0:
        out_file = 'albus_report_parallel_' + processing_option  + '_'+ MSname
      else:
        out_file = 'albus_report_parallel_' + processing_option

    if len(object) > 0:
      out_file = out_file + '_' + object

    # get rid of uppercase characters because MeqTrees PyNodes cannot
    # handle them
    # out_file = out_file.lower()

    if os.path.exists(out_file):
      os.remove(out_file)
    log = open(out_file, 'a')

# setting num_processors == 1 seems to work better for getting GPS data from
# Geosciences Australia. Anyway most time for Australia data is spent at the
# bias / clock correction fitting stuff

    MS,start_time,end_time, lat_set,Lat1,Long1,Ht1 = setup_AlbusIonosphere_for_ref_date(log,MSname,MSdir,Lat,Long,Height,start_time,end_time, object,time_step,telescope_pos,station_pos_x,station_pos_y,station_pos_z,processing_option,tolerance,overwrite,do_serial,raise_bias_error,num_processors,use_pim,use_global_data, gps_data_directory)

# can we use more than two processors for Ionosphere TEC / RM predictions?
    if num_processors <= 2:
      try:
        import multiprocessing
        processors =  multiprocessing.cpu_count()
        if processors > num_processors:
          num_processors = processors
          print( '*** setting number of processors to',num_processors )
      except:
        pass

    if lat_set:
      Lat = math.degrees(Lat1)
      Long = math.degrees(Long1)
      Height = math.degrees(Ht1)
      Lat = Lat1
      Long = Long1
      Height = Ht1
      print( 'setting Latitude, Longitude, Height to ', Lat, Long, Height)
      print>>log, 'setting Latitude, Longitude, Height to ', Lat, Long, Height
 
    if Ra == 0 and Dec == 0 and len(MS) > 0: 
      direction = get_observing_position(MS)
      print( 'observation direction ', direction)
      print(>>log, 'observation direction ', direction)
      retval = Iono_agw.set_source_position(direction[0],direction[1])
    else:
      try:
        (Ra,Dec)=ac.radec_str_to_rad2(Ra,Dec)
        if El < 0.0:
          print(>>log, 'observation direction ', Ra, Dec)
        else:
          print(>>log, 'observation in fixed Az El direction ', Az, El)
      except:
        if El < 0.0:
          print(>>log, 'observation direction ', Ra, Dec)
        else:
          print(>>log, 'observation in fixed Az El direction ', Az, El)
      retval = Iono_agw.set_source_position(Ra, Dec)

    print( "source",retval)

    location_ra = []
    location_dec = []
    numi=Iono_agw.get_Num_Ionospheric_Predictions()
    print( '*** number of predictions: ', numi)
    # make sure global location lists are reset to empty
    if El >= 0.0:
      print( 'time step ', time_step)
      print( 'fixed azimuth and elevation are ', Az, El)
      print( 'at location Lat, Long, height ', Lat, Long, Height)
#     HAS_PYRAP = False
      if HAS_PYRAP:
        me=measures();
        if type(Az)!=str:
            az=str(Az)+'deg';
        if type(El)!=str:
            el=str(El)+'deg';
        obsdir=me.direction('AZELGEO',az,el)
        print( Long, Lat, Height)
        try:
          p = me.position('WGS84',str(Long)+'rad', str(Lat)+'rad', str(Height) +'m')
        except:
          Long = ac.deg_str_to_rad(Long)
          Lat = ac.deg_str_to_rad(Lat)
          p = me.position('WGS84',str(Long)+'rad', str(Lat)+'rad', str(Height) +'m')
        me.do_frame(p);
        loc_time = start_time * 86400.0 - time_step
        location_time =  str(loc_time) + 's'
        print( 'starting location_time ', location_time )
        t=me.epoch("UTC",qu.quantity(location_time));
        print( 'pyrap time ', t)
#  myme.doframe(myme.epoch('mjd', qa.quantity(mjd, 'd')))
#   myme.doframe(myme.observatory(observatory))
#   myradec = myme.measure(mydir,'J2000')
        me.do_frame(t);
        for i in range(numi):
#         radec = me.measure(obsdir,'RADEC');
#  the following call gives the same result as the one above
          radec = me.measure(obsdir,'J2000');
          ra=radec['m0']['value'];
          dec=radec['m1']['value'];
#         print( 'i pyrap ra, dec ', i, math.degrees(ra),math.degrees(dec))
          location_ra.append(ra)
          location_dec.append(dec)
          loc_time = loc_time + time_step
          location_time =  str(loc_time) + 's'
          t=me.epoch("UTC",qu.quantity(location_time));
          me.do_frame(t);
      elif HAS_EPHEM:
        observer = ephem.Observer()
        observer.lat = Lat 
        observer.lon = Long
        observer.elevation = Height
        observer.pressure = 0.0
#       print( 'observer contents', observer)
        # convert to Dublin Julian Date for PyEphem and 
        # subtract 300 sec for first integration because of offset 
        # in data_calibrator.cxx code
#       print( 'start_time dublin time ', start_time, start_time - 15019.5 )
        observer.date =  start_time - 15019.5 - time_step/86400.0
        az = str(Az)
        el = str(El)
#       print( 'az el ', az, el)
        ra,dec = observer.radec_of(az, el)
        print(>>log, 'observing at a fixed azimuth and elevation (deg) of', Az,El)
        print(>>log, 'starting observation direction ', ra, dec)
        print( 'observing at a fixed azimuth and elevation (deg) of', Az,El)
        print( 'starting observation direction ', ra, dec)
        
        for i in range(numi):
          ra,dec = observer.radec_of(az, el)
#         dec = dec + math.radians(0.15)
#         print( 'i ephem ra, dec ', i, math.degrees(ra),math.degrees(dec))
          ra_float = float(ra)
          dec_float = float(dec)
          location_ra.append(ra_float)
          location_dec.append(dec_float)
          observer.date = observer.date + time_step / 86400.0
      else:
        print(>>log,'pyephem not installed - cannot compute Ra and Dec')
        print( 'pyephem not installed - cannot compute Ra and Dec')
        sys.exit(-1)
    # do ionosphere predictions in parallel
    NUMBER_OF_PROCESSES = num_processors
#   NUMBER_OF_PROCESSES = 1
    print( 'for report using NUMBER_OF_PROCESSES', NUMBER_OF_PROCESSES)
    increment = numi / NUMBER_OF_PROCESSES
    times = []
    end_pos = 0
    start_pos = 0 - increment
    for i in range(NUMBER_OF_PROCESSES):
       start_pos = start_pos + increment
       if i == NUMBER_OF_PROCESSES-1:
         end_pos = numi
       else:
         end_pos = end_pos + increment
       times.append((start_pos,end_pos))
    print( 'using times in tasks ', times)
    TASKS = [(predict_iono_corrections, (i,times,location_ra,location_dec)) for i in range(NUMBER_OF_PROCESSES)]
    print( 'number of prediction tasks ', len(TASKS))

   # Create queues
    task_queue = Queue()
    done_queue = Queue()

    # Submit tasks
    for task in TASKS:
      task_queue.put(task)

    # Start worker processes
    for i in range(NUMBER_OF_PROCESSES):
      Process(target=iono_worker, args=(task_queue, done_queue)).start()

    report_list = {}
    # Get and print( results
    for i in range(len(TASKS)):
      result = done_queue.get()
      report_list[result[0]] = result

    # Tell child processes to stop
    for i in range(NUMBER_OF_PROCESSES):
      task_queue.put('STOP')
    print( '*********** finished ionosphere predictions ***************')

    print( >> log, '     ')
    print( >> log, 'ALBUS report column explanation (zero relative):')
    print( >> log, '0 - sequence number (zero relative)')
    print( >> log, '1 - separator colon')
    print( >> log, '2 - value of 0 - valid data/calculation')
    print( >> log, '    value of 1 - invalid data/calculation (usually, but not always, because elevation < 0 )')
    print( >> log, '3 - relative time in seconds relative to reference time')
    print( >> log, '4 - time step between sequence calculations (default is 300 sec)')
    print( >> log, '5 - elevation in radians')
    print( >> log, '6 - azimuth in radians')
    print( >> log, '7 - TEC (in tec units) in the current azimuth/elevation direction')
    print( >> log, '8 - Rotation Measure (radians/m^2) in the current azimuth/elevation direction')
    print( >> log, '9 - correction factor to convert STEC to value at the zenith')
    print( >> log, '10 - formal error in STEC fit')
    print( >> log, '     ')

    # report results
    print( >> log, 'seq  rel_time time_width El         Az         STEC           RM (rad/m2)   VTEC factor   STEC_error')
    for i in range(NUMBER_OF_PROCESSES):
      data_list = report_list[i]
      num_to_report = len(data_list)
      for j in range(1,num_to_report):
        data = data_list[j]
        print(>>log,  data[0],':',data[1],data[2], data[3], data[4], data[5],data[6], data[7], data[8], data[9])

    print( ' ')
    print( '*********** finished task!! ***************')

    stoptime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print( "Stop at %s" % stoptime)
    process_end = time.time()
    duration = (process_end - process_start)/3600.0
    print( "process_ionosphere: total run time: %7.2f hours" % duration)

if __name__ == "__main__":

# Maaijke LOFAR observation
# RA=2.153741
# DEC=0.841552
# END_TIME=4861053503.801514
# START_TIME=4861047495.461060 
  OBJECT="LOFAR_TEST" 

  OBJECT="LOFAR_OBS" 

# position of LOFAR CS002LBA from CASA Observatories file
  LAT="52:54:54.4"
  LONG="06:52:11.4"
  HEIGHT=49.366

# STN_POS_X = [3847753.310000, 3783537.525000, 3826896.235000]
# STN_POS_Y = [466962.809000, 450130.064000, 460979.455000]
# STN_POS_Z = [5048397.2440, 5097866.1460,  5064658.2030]
# STN_POS_X = [3847753.310000]
# STN_POS_Y = [466962.809000]
# STN_POS_Z = [5048397.2440]
  EL = 90.0
  TIME_STEP = 300.0

# note: the following form of date/time is required if 
# you only have pyephem installed
  START_TIME="2013/12/21 00:00:00"
  END_TIME="2013/12/21 23:59:59"

# process_ionosphere(Ra=RA,Dec=DEC,station_pos_x=STN_POS_X,station_pos_y=STN_POS_Y,station_pos_z=STN_POS_Z,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=1,num_processors=15, gps_data_directory="GPS_data_for_LOFAR_test",object=OBJECT)
# print( ' ******* STARTING UP ********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0,num_processors=8, gps_data_directory="LOFAR_GPS_data_Dec_2013",time_step=TIME_STEP,object=OBJECT)

##########################################

# process_ionosphere(MSname="ASKAP_iono_test.MS", MSdir=".",processing_option="RI_G03",do_serial=0,num_processors=4,gps_data_directory="store_albus_data_ASKAP");


# Sobey pulsar experiment
  RA="08:37:05.642"
  DEC="+06:10:14.56"
  LAT="52:54:54.4"
  LONG="06:52:11.6"
  HEIGHT=49.3
  START_TIME="11apr2011/16:40:0"
# END_TIME="11apr2011/23:00:0"
  END_TIME="11apr2011/18:00:0"
  OBJECT="Sobey_Pulsar_1"
#  
# process_ionosphere(object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=1,num_processors=6, gps_data_directory="Sobey_deepcopy");


# Sobey second pulsar experiment
# RA="08:37:05.642"
# DEC="+06:10:14.56"
# LAT="52:54:54.4"
# LONG="06:52:11.6"
# HEIGHT=49.3
# START_TIME="20oct2011/04:00:0"
# END_TIME="20oct2011/08:00:0"
# time formats if using pyephem. NOTE: pyrap quantity also handles this format
# START_TIME="2011/10/20 04:00:0"
# END_TIME="2011/10/20 08:00:0"
# OBJECT="PSR_B0834+06" 

# process_ionosphere(Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=1,num_processors=6, gps_data_directory="Sobey_2",object=OBJECT)

# MWA test
# RA="08:37:05.642"
# DEC="-60:10:14.56"
# time formats if using pyephem. NOTE: pyrap quantity also handles this format
# START_TIME="2011/10/20 04:00:0"
# END_TIME="2011/10/20 08:00:0"
# OBJECT="MWA_sky" 

  RA="06:36:32.5"
  DEC="-20:42:18.0"
# position below is that of MRO1 GPS station
  LONG="116:38:15.0"
  LAT="-26:41:47.9"
  HEIGHT=354.1
# END_TIME="2015/10/18 23:57:06"
  END_TIME="2015/10/18 14:39:06"

# START_TIME="2013/08/26 00:00:00"
# END_TIME="2013/08/26 23:59:59"
  START_TIME="2015/03/28 00:00:00"
  END_TIME="2015/03/28 23:59:59"
  START_TIME="2014/03/04 00:00:00"
  END_TIME="2014/03/04 23:59:59"
  START_TIME="2016/11/15 00:00:00"
  END_TIME="2016/11/15 23:59:59"
  START_TIME="2012/10/18 12:19:06"
  END_TIME="2012/10/18 21:57:06"
  START_TIME="2017/02/25 00:00:00"
  END_TIME="2017/02/25 23:59:59"
  OBJECT="MWA_Feb2_2917_EL_90deg" 
  OBJECT="MWA_Feb2_2917_PIM_track" 


  TIME_STEP = 300.0

  MWA_antennas = numpy.array([[-2559314.23084924,5095535.90961438,-2848889.57667157],
   [-2559293.10717106,5095498.79164383,-2848974.05801863],
   [-2559156.42269442,5095418.83230373,-2849233.34162414],
   [-2559094.63804600,5095526.84526066,-2849097.63284488],
   [-2559042.54106487,5095566.88538445,-2849072.42535023],
   [-2559068.53757350,5095654.59288871,-2848892.60844473],
   [-2559161.70851932,5095607.73286033,-2848894.91011893],
   [-2559173.78330034,5095643.10464650,-2848820.20086397],
   [-2559782.26851932,5095304.54438001,-2848875.78669410],
   [-2559644.22829760,5095369.93521424,-2848885.21756417],
   [-2559507.77695003,5095490.23883646,-2848797.39833977],
   [-2559467.43177484,5095508.01973328,-2848802.30233654],
   [-2559460.59086333,5095515.74910944,-2848794.76371318],
   [-2559491.68457220,5095527.67486954,-2848745.44773601],
   [-2559603.60609646,5095563.73884050,-2848579.72258876],
   [-2559631.28428317,5095541.41922988,-2848594.98830325],
   [-2559113.92023486,5095854.59042124,-2848492.05455485],
   [-2559133.51844911,5095831.00304170,-2848517.14718873],
   [-2559018.96896708,5095793.67783611,-2848686.69023686],
   [-2558906.48396095,5095592.28259425,-2849148.93562390],
   [-2558894.77687225,5095720.00453191,-2848930.82517056],
   [-2558880.58102582,5095762.06255238,-2848868.27661380],
   [-2558503.88881043,5095891.11710898,-2848981.31195756],
   [-2558648.85477276,5096060.47633611,-2848544.49069260],
   [-2558998.73468649,5095390.06352995,-2849423.09595365],
   [-2559238.04568324,5095263.75775157,-2849432.88470164],
   [-2558856.49159020,5095257.96516587,-2849788.57821277],
   [-2558761.92575271,5095281.91134845,-2849829.99130606],
   [-2558719.21221208,5095416.28342253,-2849628.99110746],
   [-2558836.79342206,5095555.42415917,-2849277.33903756],
   [-2558850.45931999,5095586.71918979,-2849209.71070222],
   [-2558890.31919482,5095521.92810583,-2849288.42518348]])

  process_ionosphere(telescope_pos=MWA_antennas,Ra=RA,Dec=DEC,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=1,num_processors=1,raise_bias_error=0,gps_data_directory="/home/twillis1/MWA_Beta2_agw_data_store",use_pim=1,object=OBJECT)
# process_ionosphere(telescope_pos=MWA_antennas,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0, num_processors=8, raise_bias_error=0, gps_data_directory="/home/twillis1/MWA_EoR0_data_store",use_global_data=0,object=OBJECT)
# process_ionosphere(telescope_pos=MWA_antennas,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0, num_processors=8, raise_bias_error=0, gps_data_directory="/home/twillis1/MWA_EoR0_data_store",use_global_data=0,object=OBJECT)
  EL=90
  OBJECT="MWA_Feb2_2917_PIM_EL_90deg" 
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=1, num_processors=1,raise_bias_error=0, gps_data_directory="/home/twillis1/MWA_Beta2_agw_data_store",use_global_data=0,use_pim=1,object=OBJECT)

