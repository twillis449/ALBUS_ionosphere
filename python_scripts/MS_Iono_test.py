
from MS_Iono_functions import * 

# The following function should be the only one which needs changes that depend
# on the type of observation that one is doing

def process_ionosphere(MSname="",MSdir=".", Ra=0, Dec=0, Az=180.0, El=-90.0, Lat=0, Long=0, Height=0,start_time="",end_time="", object="",time_step=300.0,telescope_pos=None,station_pos_x=0,station_pos_y=0,station_pos_z=0,processing_option="MO_PIM",tolerance=0.1,overwrite=0,do_serial=0,raise_bias_error=1, num_processors=4,use_pim=1,use_global_data=0,gps_data_directory="."):

# Inputs:
# MSname - name of Measurement Set (MS) to use
# MSdir  - name of directory where MS is stored. Default = current
# Ra     - Right Ascension of field centre. Default = get it from MS
# Dec    - Declination of field centre. Default = get it from MS
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
#   has not been corrected for

    os.system('date')
    process_start = time.time()
    startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print "process_ionosphere Start at %s" % startime

    if len(MSname) == 0:
        print '*************'
        print ' No measurement set specified - assuming that specifications'
        print ' can be obtained from command line'
        print '*************'

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
          print '*** setting number of processors to',num_processors 
      except:
        pass
    if lat_set:
      Lat = math.degrees(Lat1)
      Long = math.degrees(Long1)
      Height = math.degrees(Ht1)
      Lat = Lat1
      Long = Long1
      Height = Ht1
      print 'setting Latitude, Longitude, Height to ', Lat, Long, Height
      print>>log, 'setting Latitude, Longitude, Height to ', Lat, Long, Height
    if Ra == 0 and Dec == 0 and len(MS) > 0: 
      direction = get_observing_position(MS)
      print 'observation direction ', direction
      print>>log, 'observation direction ', direction
      retval = Iono_agw.set_source_position(direction[0],direction[1])
      print "source",retval
    else:
      if El < 0.0:
        print>>log, 'observation direction ', Ra, Dec
        (Ra,Dec)=ac.radec_str_to_rad2(Ra,Dec)
        retval = Iono_agw.set_source_position(Ra, Dec)
      else:
        print>>log, 'observation in fixed Az El direction ', Az, El

    location_ra = []
    location_dec = []
    numi=Iono_agw.get_Num_Ionospheric_Predictions()
    print '*** number of predictions: ', numi
    # make sure global location lists are reset to empty
    if El >= 0.0:
      print 'time step ', time_step
      print 'fixed azimuth and elevation are ', Az, El
      print 'at location Lat, Long, height ', Lat, Long, Height
#     HAS_PYRAP = False
      if HAS_PYRAP:
        me=measures();
        if type(Az)!=str:
            az=str(Az)+'deg';
        if type(El)!=str:
            el=str(El)+'deg';
        obsdir=me.direction('AZELGEO',az,el)
        print Long, Lat, Height
        try:
          p = me.position('WGS84',str(Long)+'rad', str(Lat)+'rad', str(Height) +'m')
        except:
          Long = ac.deg_str_to_rad(Long)
          Lat = ac.deg_str_to_rad(Lat)
          p = me.position('WGS84',str(Long)+'rad', str(Lat)+'rad', str(Height) +'m')
        me.do_frame(p);
        loc_time = start_time * 86400.0 - time_step
        location_time =  str(loc_time) + 's'
        print 'starting location_time ', location_time 
        t=me.epoch("UTC",qu.quantity(location_time));
        print 'pyrap time ', t
        me.do_frame(t);
        for i in range(numi):
#         radec = me.measure(obsdir,'RADEC');
#  the following call gives the same result as the one above
          radec = me.measure(obsdir,'J2000');
          ra=radec['m0']['value'];
          dec=radec['m1']['value'];
          print 'i pyrap ra, dec ', i, math.degrees(ra),math.degrees(dec)
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
        print 'observer contents', observer
        # convert to Dublin Julian Date for PyEphem and 
        # subtract 300 sec for first integration because of offset 
        # in data_calibrator.cxx code
        print 'start_time dublin time ', start_time, start_time - 15019.5 
        observer.date =  start_time - 15019.5 - time_step/86400.0
        az = str(Az)
        el = str(El)
        print 'az el ', az, el
        ra,dec = observer.radec_of(az, el)
        print>>log, 'observing at a fixed azimuth and elevation (deg) of', Az,El
        print>>log, 'starting observation direction ', ra, dec
        print 'observing at a fixed azimuth and elevation (deg) of', Az,El
        print 'starting observation direction ', ra, dec
        
        for i in range(numi):
          ra,dec = observer.radec_of(az, el)
          ra_float = float(ra)
          dec_float = float(dec)
          location_ra.append(ra_float)
          location_dec.append(dec_float)
          observer.date = observer.date + time_step / 86400.0
      else:
        print>>log,'pyephem not installed - cannot compute Ra and Dec'
        print 'pyephem not installed - cannot compute Ra and Dec'
        sys.exit(-1)
    # do ionosphere predictions in parallel
    NUMBER_OF_PROCESSES = num_processors
#   NUMBER_OF_PROCESSES = 1
    print 'for report using NUMBER_OF_PROCESSES', NUMBER_OF_PROCESSES
    increment = numi / NUMBER_OF_PROCESSES
    times = []
    end_pos = 0
    start_pos = 0 - increment
    try:
      for i in range(NUMBER_OF_PROCESSES):
        start_pos = start_pos + increment
        print 'i start_pos', i, start_pos
        if i == NUMBER_OF_PROCESSES-1:
          end_pos = numi
        else:
          end_pos = end_pos + increment
        print 'i end_pos', i, end_pos
        times.append((start_pos,end_pos))
    except:
      pass
    print 'using times in tasks ', times

    TASKS = [(predict_iono_corrections, (i,times,location_ra,location_dec)) for i in range(NUMBER_OF_PROCESSES)]
    print 'number of prediction tasks ', len(TASKS)

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
    # Get and print results
    for i in range(len(TASKS)):
      result = done_queue.get()
      report_list[result[0]] = result

    # Tell child processes to stop
    for i in range(NUMBER_OF_PROCESSES):
      task_queue.put('STOP')
    print '*********** finished ionosphere predictions ***************'

    print >> log, '     '
    print >> log, 'ALBUS report column explanation (zero relative):'
    print >> log, '0 - sequence number (zero relative)'
    print >> log, '1 - separator colon'
    print >> log, '2 - value of 0 - valid data/calculation'
    print >> log, '    value of 1 - invalid data/calculation (usually, but not always, because elevation < 0 )'
    print >> log, '3 - relative time in seconds relative to reference time'
    print >> log, '4 - time step between sequence calculations (default is 300 sec)'
    print >> log, '5 - elevation in degrees'
    print >> log, '6 - azimuth in degrees'
    print >> log, '7 - TEC (in tec units) in the current azimuth/elevation direction'
    print >> log, '8 - Rotation Measure (radians/m^2) in the current azimuth/elevation direction'
    print >> log, '9 - correction factor to convert STEC to value at the zenith'
    print >> log, '10 - formal error in STEC fit (personally I find this error to be too '
    print >> log, '     large but I am not sure how to adjust it)'
    print >> log, '     '

    # report results
    print >> log, 'seq  rel_time time_width El         Az         STEC           RM (rad/m2)   VTEC factor   STEC_error'
    for i in range(NUMBER_OF_PROCESSES):
      data_list = report_list[i]
      num_to_report = len(data_list)
      for j in range(1,num_to_report):
        data = data_list[j]
        print>>log,  data[0],':',data[1],data[2], data[3], math.degrees(data[4]), math.degrees(data[5]),data[6], data[7], data[8], data[9]

    print ' '
    print '*********** finished task!! ***************'

    stoptime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print "Stop at %s" % stoptime
    process_end = time.time()
    duration = (process_end - process_start)/60.0
    print 'process_ionosphere: total run time: %7.2f minutes' % duration
    print >> log, 'process_ionosphere: total run time: %7.2f minutes' % duration

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
# print ' ******* STARTING UP ********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0,num_processors=8, gps_data_directory="LOFAR_GPS_data_Dec_2013",time_step=TIME_STEP,object=OBJECT)

##########################################

# process_ionosphere(MSname="ASKAP_iono_test.MS", MSdir=".",processing_option="RI_G03",do_serial=0,num_processors=4,gps_data_directory="store_albus_data_ASKAP");


# Sobey pulsar experiment
  RA="08:37:05.642"
  DEC="+06:10:14.56"

  RA = "03:34:06.15"
  DEC = "+54:37:37.25"

  LAT="52:54:54.4"
  LONG="06:52:11.6"
  HEIGHT=49.3
# END_TIME="17jul2015/23:00:0"
  START_TIME="17jul2015/03:30:00"
  END_TIME="07sep2015/11:30:0"
  START_TIME="2015/07/17 03:30:00"
  END_TIME="2015/07/17 11:30:00"
  OBJECT="Pulsar_B0329+54_test"
  TIME_STEP = 300.0
#  
# print ' ******* STARTING UP Van Eck********'
# process_ionosphere(time_step=TIME_STEP,object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=0,num_processors=8, gps_data_directory="Sobey_test");


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
# START_TIME="2012/10/18 12:19:06"
# END_TIME="2012/10/18 21:57:06"
# END_TIME="2015/10/18 23:57:06"
  END_TIME="2015/10/18 14:39:06"

  EL=90
  OBJECT="EoR0"
# START_TIME="2013/08/26 00:00:00"
# END_TIME="2013/08/26 23:59:59"
  START_TIME="2015/03/28 00:00:00"
  END_TIME="2015/03/28 23:59:59"
  START_TIME="2014/03/04 00:00:00"
  END_TIME="2014/03/04 23:59:59"
  START_TIME="2018/01/15 00:00:00"
  END_TIME="2018/01/15 23:59:59"
  OBJECT="MWA_Jan_2018"


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

  MWA_mean_pos = [[-2559129.58799, 5095564.16506, -2848999.9728]]

# process_ionosphere(telescope_pos=MWA_antennas,Ra=RA,Dec=DEC,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=1,num_processors=6, gps_data_directory="MWA_Beta2_agw_data_store",object=OBJECT)
# process_ionosphere(telescope_pos=MWA_antennas,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0, num_processors=8, raise_bias_error=0, gps_data_directory="/home/twillis1/MWA_EoR0_data_store",use_global_data=0,object=OBJECT)
# process_ionosphere(telescope_pos=MWA_antennas,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0, num_processors=8, raise_bias_error=0, gps_data_directory="/home/twillis1/MWA_EoR0_data_store",use_global_data=0,object=OBJECT)
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=1, num_processors=8,raise_bias_error=0, gps_data_directory="/home/twillis/twillis1/MWA_NOV_2016",use_global_data=0,object=OBJECT)

  objectName = "MWA_BrightLoqRM"
  RA="23:49:18.049"
  DEC="-32:24:23.39"
  TIME_STEP = 300.0
  OBJECT="eor_2456967_%s" %(objectName)
  START_TIME="2014/11/06 00:00:00"
  END_TIME="2014/11/06 23:59:59"

# process_ionosphere(telescope_pos=MWA_antennas,start_time=START_TIME,end_time=END_TIME,Ra=RA,Dec=DEC,processing_option="RI_G03",do_serial=0, use_global_data=0, gps_data_directory="MWA_eor_test_data_store",time_step=TIME_STEP,object=OBJECT)





# DRAO experiment
  LAT="49:19:21.425"
  LONG="-119:37:29.94"
  HEIGHT= 541.878
  START_TIME="2017/12/09 00:00:00"
  END_TIME="2017/12/09 23:59:59"
  RA = "00:00:00.00"
  DEC = "89:59:58.00"
  OBJECT="DRAO_CHIME_simple_test"

  TIME_STEP = 300.0
  print ' ******* STARTING UP DRAO CHIME TEST ********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,Ra=RA,Dec=DEC,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=0,num_processors=8, gps_data_directory="DRAO_simple_chime_Dec_2017",time_step=TIME_STEP,raise_bias_error=0,object=OBJECT)
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=1,num_processors=8, gps_data_directory="elevation_test",object=OBJECT)

# process_ionosphere(MSname="3C286_MAY15.MS",Lat=LAT,Long=LONG,Height=HEIGHT,MSdir="/home/twillis/ASKAP_related_ionosphere",processing_option="RI_G03",do_serial=0,num_processors=6,gps_data_directory="store_3C286_may15");

# Hat Creek experiment
# LAT="40:49:2.30"
# LONG="-121:28:18.49"
# HEIGHT= 1008.0
# Owens Valley
  LAT = "37:14:02.0"
  LONG = "-118:16:56.0"
  HEIGHT = 1222.0
  START_TIME="2013/03/11 08:00:00"
  END_TIME="2013/03/12 08:00:00"
  OBJECT="OVRO_Dummy" 
  EL = 90.0
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0,num_processors=1, gps_data_directory="/home/twillis1/OVRO_test",object=OBJECT)
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0,num_processors=8, gps_data_directory="elevation_test",object=OBJECT)

# South Africa experiment
  LONG="21:15:27.3"
  LAT="-28:24:26.0"
  HEIGHT= 868.7
  OBJECT="UPINGTON_JUNE"
  OBJECT="UPINGTON_MARCH"
  OBJECT="UPINGTON_DEC_30s"
# OBJECT="UPINGTON_DEC_2013"
  OBJECT="UPINGTON_DEC"
  OBJECT="UPINGTON_SEPT"
  OBJECT="UPINGTON_MAY_2005_LOCAL"
  OBJECT="UPINGTON_Nov_2014"
# LAT="-33:19:12.2" 
# LONG="26:30:25.8"
# OBJECT="GRAHAMSTOWN"
# HEIGHT= 674.6
# LONG="117:30:00.0"
# LAT="-26:36:00.0"
# HEIGHT= 200.0   # guessed
# OBJECT="AU_TEST"
  START_TIME="2006/06/22 00:00:00"
  END_TIME="2006/06/22 23:59:59"
  START_TIME="2006/03/15 00:00:00"
  END_TIME="2006/03/15 23:59:59"
# START_TIME="2013/12/21 00:00:00"
# END_TIME="2013/12/21 23:59:59"
  START_TIME="2006/12/21 00:00:00"
  END_TIME="2006/12/21 23:59:59"
  START_TIME="2006/09/21 00:00:00"
  END_TIME="2006/09/21 23:59:59"
  START_TIME="2005/05/05 00:00:00"
  END_TIME="2005/05/05 23:59:59"
  START_TIME="2014/11/06 00:00:00"
  END_TIME="2014/11/06 23:59:59"
  START_TIME="2005/05/05 00:00:00"
  END_TIME="2005/05/05 23:59:59"
# START_TIME="2014/11/06 00:00:00"
# END_TIME="2014/11/06 23:59:59"
  EL = 90.0
  TIME_STEP = 300.0

# OBJECT = "SUTHERLAND_2004"
# LONG="20:48:37.7"
# LAT ="-32:22:48.8"
# HEIGHT= 1799.8
# START_TIME="2004/11/07 00:00:00"
# END_TIME="2004/11/11 23:59:59"
  print ' ******* STARTING UP UPINGTON May 05 ********'

  OBJECT="Upington_IONO_2015_later" 
  START_TIME="2014/08/17 00:00:00"
  END_TIME="2014/08/17 23:59:59"
  START_TIME="2015/09/17 00:00:00"
  END_TIME="2015/09/17 23:59:59"
  EL=90
  TIME_STEP = 300.0
  RA="19:40:45.01"
  DEC="-50:00:00.00"

# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0,num_processors=1, gps_data_directory="/home/twillis1/Upington_iono_2015_later",time_step=TIME_STEP,object=OBJECT)
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0,num_processors=8, gps_data_directory="/home/twillis1/Upington_GPS_data_Nov",time_step=TIME_STEP,object=OBJECT,use_global_data=1)
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",use_global_data=0,do_serial=1,num_processors=8, gps_data_directory="/home/twillis/Upington_Nov_2014_data",time_step=TIME_STEP,object=OBJECT)

# Australia 2001 experiment
  LAT="-35:22:48.0"
  LONG="148:58:12.0"
  HEIGHT= 665.4     # who knows ... 148 58 48.0 -35 23 57.1   665.4
  OBJECT="TIDBINBILLA_2001"
  START_TIME="2001/03/31 00:00:00" 
  END_TIME="2001/03/31 23:59:59"
# LAT="-42:48:17.0"
# LONG="147:26:19.4"
# HEIGHT= 41.1     # who knows ... 147 26 19.4 -42 48 17.0    41.1
# OBJECT="HOBART"
# START_TIME="2011/03/31 00:00:00"
# END_TIME="2011/03/31 01:59:59"
  EL = 90.0
# print ' ******* STARTING UP ********,'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="MO_IRI",do_serial=0,num_processors=8, gps_data_directory="elevation_test",object=OBJECT)
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0,num_processors=8, gps_data_directory="tidbinbilla_test",object=OBJECT)

# Millstone Hill obs -  reference:http://mwa-lfd.haystack.mit.edu/mwatelescope/science/shi/is.html
  LONG="288:30:29.77"
  LAT="42:37:10.2"
  HEIGHT= 146.0
  OBJECT="MILLSTONE_HILL_ALL_GPS_no_th_model"
  START_TIME="2006/12/13 00:00:00"
  END_TIME="2006/12/15 23:59:59"
# START_TIME="2006/12/15 00:00:00"
# END_TIME="2006/12/15 23:59:59"
  EL = 90.0
  TIME_STEP = 300.0

# reference = Huang et al 2005 A strone positive phase of ionospherric storms ...
# OBJECT="MILLSTONE_HILL_Apr1"
# START_TIME="2004/04/01 00:00:00"
# END_TIME="2004/04/03 23:59:59"
# TIME_STEP = 300.0
  print ' ******* STARTING UP Millstone Hill ********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="MO_PIM",do_serial=0,num_processors=8, gps_data_directory="elevation_test",time_step=TIME_STEP,object=OBJECT)
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=1,num_processors=1, gps_data_directory="/home/twillis1/Millstone_hill_All",time_step=TIME_STEP,use_global_data= 0, object=OBJECT)

# Korea 2010 - SUWN station
  LONG="127:03:15.3"
  LAT="37:16:31.8"
  HEIGHT= 82.3
  OBJECT="KOREA_DO_SER"
  START_TIME="2010/01/01 00:00:00"
  END_TIME="2010/01/03 23:59:59"
  EL = 90.0
  TIME_STEP = 300.0
  print ' ******* STARTING UP KOREA TEST********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="MO_IRI",do_serial=1,num_processors=8, gps_data_directory="/home/twillis1/ALBUS_korea_test",time_step=TIME_STEP,object=OBJECT)

# Istanbul 2003 -  ista station
  LONG="29:01:09.5"
  LAT="41:06:15.8"
  HEIGHT= 147.2
  OBJECT="ISTANBUL_Local"
  START_TIME="2003/10/10 00:00:00"
  END_TIME="2003/10/10 23:59:59"
# Ankara 2003 -  ankr station
  LONG="32:45:30.9"
  LAT="39:53:15.0"
  HEIGHT= 974.8
  OBJECT="ANKARA_Local"
  START_TIME="2003/10/28 00:00:00"
  END_TIME="2003/10/28 23:59:59"
  EL = 90.0
  TIME_STEP = 300.0
  print ' ******* STARTING UP Ankara TEST********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=1,num_processors=1, use_global_data= 0, gps_data_directory="/home/twillis1/ALBUS_Ankara_test",time_step=TIME_STEP,object=OBJECT)

# Germany -  wtzr station = location of Wettzell Geodetic VLBI station
  LONG="12:52:44.1"
  LAT="49:08:39.1"
  HEIGHT= 666.0
  OBJECT="WETTZELL_long"
  OBJECT="WETTZELL_all"
# START_TIME="2002/10/17 00:00:00"
# END_TIME="2002/10/31 23:59:59"
  START_TIME="2002/10/25 00:00:00"
  END_TIME="2002/10/25 23:59:59"
  EL = 90.0
  TIME_STEP = 300.0
  print ' ******* STARTING UP WETTZELL TEST********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0,num_processors=1, use_global_data= 1, gps_data_directory="/home/twillis1/ALBUS_Wettzell_test",time_step=TIME_STEP,object=OBJECT)

# Algonquin Park Test
  LONG="281:55:37.0"
  LAT="45:57:19.8"
  HEIGHT= 260.4
  OBJECT="ARO_Pulsar_B0329+54_Aug1_transit"
  OBJECT="ARO_Pulsar_B0329+54_test"
  RA = "03:34:06.15"
  DEC = "54:37:37.25"
  START_TIME="2014/08/02 19:05:00"
  END_TIME="2014/08/02 19:25:00"
  START_TIME="2014/08/02 11:45:00"
  END_TIME="2014/08/02 12:45:00"
  START_TIME="2014/08/01 19:01:00"
  END_TIME="2014/08/01 19:21:00"
  START_TIME="2014/08/01 11:41:00"
  END_TIME="2014/08/01 12:41:00"
  START_TIME="2016/11/10 00:00:00"
  END_TIME="2016/11/10 23:59:59"
  TIME_STEP = 300.0
  print ' ******* STARTING UP ALGONQUIN TEST********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,Ra=RA,Dec=DEC,processing_option="RI_G03",do_serial=1,num_processors=8, raise_bias_error=1,use_global_data=0, gps_data_directory="/home/twillis1/ALBUS_ALGONQUIN_test",time_step=TIME_STEP,object=OBJECT)

# MWA IONO test using times from Coster et al 2012
  OBJECT="MWA_IONO_coster" 
  START_TIME="2011/09/26 00:00:00"
  END_TIME="2011/09/27 23:59:59"
  START_TIME="2014/12/01 00:00:00"
  END_TIME="2014/12/01 23:59:59"
  EL=90
  TIME_STEP = 300.0

  OBJECT="MWA_IONO_later" 
  START_TIME="2015/08/17 00:00:00"
  END_TIME="2015/08/17 23:59:59"
  EL=90
  TIME_STEP = 300.0
  RA="19:40:45.01"
  DEC="-50:00:00.00"
  OBJECT="MWA_IONO_track_later" 

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
# print ' ******* STARTING UP MWA_IONO_TESTT********'
# process_ionosphere(num_processors=16,telescope_pos=MWA_antennas,start_time=START_TIME,end_time=END_TIME,Ra=RA,Dec=DEC,processing_option="RI_G03",do_serial=0, use_global_data=0, gps_data_directory="/home/twillis1/MWA_Iono_test_later",time_step=TIME_STEP,object=OBJECT)
# process_ionosphere(El=EL,num_processors=16,telescope_pos=MWA_antennas,start_time=START_TIME,end_time=END_TIME,Ra=RA,Dec=DEC,processing_option="RI_G03",do_serial=0, use_global_data=0, gps_data_directory="/home/twillis1/MWA_Iono_test_later",time_step=TIME_STEP,object=OBJECT)

# China Xiamen 2006 test
  LONG="118:06:0.0"
  LAT="24:30:0.0"
  HEIGHT= 260.4         # guess!
  OBJECT="XIAMEN_local_test_pim"
  START_TIME="2006/05/23 00:00:00"
  END_TIME="2006/05/25 23:59:59"
  EL = 90.0
  TIME_STEP = 300.0
# print ' ******* STARTING UP XIAMEN TEST********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=1,num_processors=4, use_global_data= 0, gps_data_directory="/home/twillis1/ALBUS_XIAMEN",time_step=TIME_STEP,object=OBJECT)
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=1,gps_data_directory="/home/twillis1/ALBUS_XIAMEN_all",time_step=TIME_STEP,object=OBJECT)

# Aussie Station MTMA 117 50 35.1  -28  6 55.1    389.5
  LONG="117:50:35.1"
  LAT="-28:06:55.1"
  HEIGHT= 389.5
  OBJECT="MTMA_coster_pim"
  START_TIME="2012/10/31 00:00:00"
  END_TIME="2012/10/31 23:59:59"
# MWA IONO test using times from Coster et al 2012
# OBJECT="MWA_MTMA_coster" 
  START_TIME="2011/09/26 00:00:00"
  END_TIME="2011/09/27 23:59:59"

  EL = 90.0
  TIME_STEP = 300.0
  print ' ******* STARTING UP MTMA TEST********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0,use_global_data=0,num_processors=1, gps_data_directory="/home/twillis1/ALBUS_MTMA",object=OBJECT)
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=1,gps_data_directory="/home/twillis1/ALBUS_MTMA_coster",object=OBJECT)

# MEERKAT IONO test 
  OBJECT="MEERKAT_IONO_RA_DEC" 
  START_TIME="2016/09/26 00:00:00"
  END_TIME="2016/09/27 23:59:59"
# EL=90
  TIME_STEP = 300.0
  RA="19:40:45.01"
  DEC="-63:40:39.81"

  MEER_antennas = numpy.array([[5109812.51305709,  2003645.39155585, -3239784.79195509]])

  print ' ******* STARTING UP MEERKAT_IONO_TESTT******** '
# process_ionosphere(telescope_pos=MEER_antennas,start_time=START_TIME,end_time=END_TIME,El=EL,processing_option="RI_G03",do_serial=0, use_global_data=0, raise_bias_error=0, num_processors=8, gps_data_directory="/home/twillis/twillis1/MEER_Iono_test",time_step=TIME_STEP,object=OBJECT)
  process_ionosphere(telescope_pos=MEER_antennas,start_time=START_TIME,end_time=END_TIME,Ra=RA,Dec=DEC,processing_option="RI_G03",do_serial=1, use_global_data=0, raise_bias_error=0, num_processors=8, gps_data_directory="/home/twillis/twillis1/MEER_Iono_test",time_step=TIME_STEP,object=OBJECT)

  # 1934-638 on 141209
  RA="19:40:45.01"
  DEC="-63:40:39.81"
  LONG="116:39:32.00"
  LAT="-26:42:15.0"
  HEIGHT= 121.35
  OBJECT = '1934-638'

  START_TIME="2014/12/09 00:00:00"
  END_TIME="2014/12/09 23:59:59"

  print ' ******* STARTING UP SAULT_IONO_TESTT******** '
# process_ionosphere(object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",use_global_data=0,do_serial=1,num_processors=1, gps_data_directory="141209_Sault")
# process_ionosphere(object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="MO_PIM",use_global_data=0,do_serial=1,num_processors=1, gps_data_directory="141209_Sault")
# process_ionosphere(object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="MO_IRI",use_global_data=0,do_serial=1,num_processors=1, gps_data_directory="141209_Sault")


# DRAO experiment
  RA = "00:00:00.00"
  DEC = "89:59:58.00"
  LAT="49:19:21.425"
  LONG="-119:37:29.94"
  HEIGHT= 541.878
  OBJECT="DRAO_CHIME_test_Oct_22_1800"
  START_TIME="2015/10/22 00:00:00"
  END_TIME="2015/10/30 23:59:59"
  print ' ******* STARTING UP ********'
# process_ionosphere(time_step=TIME_STEP,object=OBJECT,Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=1,num_processors=8, gps_data_directory="DRAO_chime_oct");

# Van ECK pulsar experiment
  OBJECT="Pulsar_B0329+54_test"
  RA = "03:34:06.15"
  DEC = "54:37:37.25"
  START_TIME="2015/07/17 03:30:00"
  END_TIME="2015/07/17 11:30:00"

# position of LOFAR CS002LBA from CASA Observatories file
  LAT="52:54:54.4"
  LONG="06:52:11.4"
  HEIGHT=49.366
  TIME_STEP = 300.0

# print ' ******* STARTING UP Van Eck Observation ********'
# process_ionosphere(Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=0,num_processors=8, gps_data_directory="Van_ECK_pulsar_data",time_step=TIME_STEP,raise_bias_error=1, object=OBJECT)

