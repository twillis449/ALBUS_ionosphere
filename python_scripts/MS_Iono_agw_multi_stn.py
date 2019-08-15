
from MS_Iono_functions import * 

# The following function should be the only one which needs changes that depend
# on the type of observation that one is doing

def process_ionosphere(MSname="",MSdir=".", Ra=0, Dec=0, Az=180.0, El=-90.0, Lat=0, Long=0, Height=0,start_time="",end_time="", object="",time_step=300.0,telescope_pos=None,station_pos_x=0,station_pos_y=0,station_pos_z=0,processing_option="MO_PIM",tolerance=0.1,overwrite=0,do_serial=0,raise_bias_error=0, num_processors=2,use_pim=1,use_global_data=0,gps_data_directory="."):

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
# num_processors - number of sub-tasks to run when getting data in parallel. Default = 2, which is OK for core2duo machine
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

# can we use more than two processors?
    if num_processors <= 2:
      try:
        import multiprocessing
        processors =  multiprocessing.cpu_count()
        if processors > num_processors:
          num_processors = processors
          print '*** setting number of processors to',num_processors 
      except:
        pass
    MS,start_time,end_time, lat_set,Lat1,Long1,Ht1 = setup_AlbusIonosphere_for_ref_date(log,MSname,MSdir,Lat,Long,Height,start_time,end_time, object,time_step,telescope_pos,station_pos_x,station_pos_y,station_pos_z,processing_option,tolerance,overwrite,do_serial,raise_bias_error,num_processors,use_pim,use_global_data, gps_data_directory)

    if Ra == 0 and Dec == 0 and len(MS) > 0: 
      direction = get_observing_position(MS)
      print 'observation direction ', direction
      print>>log, 'observation direction ', direction
      retval = Iono_agw.set_source_position(direction[0],direction[1])
    else:
      try:
        (Ra,Dec)=ac.radec_str_to_rad2(Ra,Dec)
        if El < 0.0:
          print>>log, 'observation direction ', Ra, Dec
        else:
          print>>log, 'observation in fixed Az El direction ', Az, El
      except:
        if El < 0.0:
          print>>log, 'observation direction ', Ra, Dec
        else:
          print>>log, 'observation in fixed Az El direction ', Az, El
      retval = Iono_agw.set_source_position(Ra, Dec)

    print "source",retval

    numi=Iono_agw.get_Num_Ionospheric_Predictions()
    print '*** number of predictions: ', numi
    location_ra = []
    location_dec = []
    if El >= 0.0:
      if HAS_EPHEM:
        print 'fixed azimuth and elevation are ', Az, El
        drao = ephem.Observer()
        drao.lat = Lat
        drao.lon = Long
        drao.elevation = Height
        drao.pressure = 0.0
        # convert to Dublin Julian Date for PyEphem and 
        # subtract 300 sec for first integration
        drao.date =  start_time - 15019.5 - 300.0/86400.0 
        az = str(Az)
        el = str(El)
        print 'az el ', az, el
        print>>log, 'observing at a fixed azimath and elevation (deg) of', Az,El
        print>>log, 'starting observation direction ', ra, dec
        
        for i in range(numi):
          ra,dec = drao.radec_of(az, el)
          location_ra.append(ra)
          location_dec.append(dec)
          drao.date = drao.date + time_step / 86400.0
      else:
        print>>log,'pyephem not installed - cannot compute Ra and Dec'
        print 'pyephem not installed - cannot compute Ra and Dec'
        sys.exit(-1)
    # do ionosphere predictions in parallel
    NUMBER_OF_PROCESSES = num_processors
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
    

    print 'number of stations to process ', len(station_pos_x)
    for k in range(len(station_pos_x)):
      print "checking position",k
      print >> log, ' '
      print >> log, 'Prediction for station %f %f %f '%(station_pos_x[k],station_pos_y[k],station_pos_z[k])
      Iono_agw.set_station_position(station_pos_x[k],station_pos_y[k],station_pos_z[k])
      TASKS = [(predict_iono_corrections, (i, times,location_ra,location_dec)) for i in range(NUMBER_OF_PROCESSES)]
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
      print '*********** finished ionosphere predictions for station %f %f %f '%(station_pos_x[k],station_pos_y[k],station_pos_z[k])

     # report results
      print >> log, 'seq  rel_time time_width El         Az         STEC           RM (rad/m2)   VTEC factor   STEC_error'
      for i in range(NUMBER_OF_PROCESSES):
       data_list = report_list[i]
       num_to_report = len(data_list)
       for j in range(1,num_to_report):
         data = data_list[j]
         print>>log,  data[0],':',data[1],data[2], data[3], data[4], data[5],data[6], data[7], data[8], data[9]

      print ' '
    print '*********** finished task!! ***************'

if __name__ == "__main__":

#Maaijke LOFAR observation
  RA=2.153741
  DEC=0.841552
# END_TIME=4861053503.801514
# START_TIME=4861047495.461060 
  START_TIME= 4862842927.0069504
  END_TIME=4862871716.9716263
  OBJECT="L80897" 
  STN_POS_X = [3847753.310000, 3783537.525000, 3826896.235000]
  STN_POS_Y = [466962.809000, 450130.064000, 460979.455000]
  STN_POS_Z = [5048397.2440, 5097866.1460,  5064658.2030]
  
  # pos=[ 3826896.235,   460979.455,  5064658.203]
  # STN_POS_X = pos[0]
  # STN_POS_Y = pos[1]
  # STN_POS_Z = pos[2]

  process_ionosphere(Ra=RA,Dec=DEC,station_pos_x=STN_POS_X,station_pos_y=STN_POS_Y,station_pos_z=STN_POS_Z,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=1,num_processors=1, gps_data_directory="GPS_data_for_LOFAR_test",object=OBJECT)

