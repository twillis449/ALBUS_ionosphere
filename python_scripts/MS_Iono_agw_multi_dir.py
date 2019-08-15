#
from MS_Iono_functions import *

#########################################################################
def get_askap_beam_locations( filename ):
# decode ASKAP file having lines with form: 0  (-0.450  0.450)  22:10:35.412,-44:49:50.70
# we want the last two coordinates
  text = open(filename, 'r').readlines()
# skip first line
# get actual data
  start = 1
  positions_ascii = []
  for i in range( start,len(text)):
    info = text[i].split(',')         # gets declination
    info1 = info[0].split(' ')        # gets right ascension
    x = len(info1)-1
    positions_ascii.append([info1[x],info[1]])

  return positions_ascii

#########################################################################

# The following function should be the only one which needs changes that depend
# on the type of observation that one is doing

def process_ionosphere_multi_dir(MSname="",MSdir=".", Ra=0, Dec=0, Az=180.0, El=-90.0, Lat=0, Long=0, Height=0,start_time="",end_time="", object="iono",time_step=300.0,telescope_pos=None,station_pos_x=0,station_pos_y=0,station_pos_z=0,max_dist=350E3,processing_option="RI_G03",tolerance=0.1,overwrite=0,do_serial=0,raise_bias_error=0, num_processors=2,use_pim=1,use_global_data=0,gps_data_directory="./",positions_file=None):

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
# num_processors - number of sub-tasks to run when getting data in parallel. Default = 2
# max_dist = maximum distance in metres from telescope location to search for gps_stations

# use_pim - 1 Use PIM model (default) in fit, 0: Use IRI model
# use_global_data - 1: include data from global GNSS network, 0: ignore data from global network
# gps_data_directory - directory where the GPS data is to be stored. Default = current one.
# positions_file - ascii data file of ASKAP focal plane array pointing positions

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


# get start time
    os.system('date')
    process_start = time.time()
    startime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print ("process_ionosphere Start at %s" % startime)


    if positions_file is None:
        print ('*************')
        print (' No postions given, so exiting !! ')
        print ('*************')
        return


    if len(MSname) == 0:
        print ('*************')
        print (' No measurement set specified - assuming that specifications')
        print (' can be obtained from command line')
        print ('*************')


    if do_serial:
      if len(MSname) > 0:
        out_file = 'albus_report_serial_' + processing_option + '_'+ MSname + '_' + object
      else:
        out_file = 'albus_report_serial_' + processing_option + '_' + object
    else:
      if len(MSname) > 0:
        out_file = 'albus_report_parallel_' + processing_option  + '_'+ MSname + '_' + object
      else:
        out_file = 'albus_report_parallel_' + processing_option + '_' + object

    if os.path.exists(out_file):
      os.remove(out_file)

    log= open(out_file, 'w')

    MS,start_time,end_time, lat_set,Lat1,Long1,Ht1 = setup_AlbusIonosphere_for_ref_date(log,MSname,MSdir,Lat,Long,Height,start_time,end_time, object,time_step,telescope_pos,station_pos_x,station_pos_y,station_pos_z,max_dist,processing_option,tolerance,overwrite,do_serial,raise_bias_error,num_processors,use_pim,use_global_data, gps_data_directory)

    print (' ')
    print ('!!!!!!!!!!!!!!!!!!!!! finished setup_AlbusIonosphere_for_ref_date !!!!!!!!!!')
    print (' ')

# can we use more than two processors?
# necessary for ASKAP, as initial data collection seems to work best with
# just one processor
    if num_processors < 8:
      try:
        import multiprocessing
        processors =  multiprocessing.cpu_count()
        if processors > num_processors:
          num_processors = processors
          print ('*** setting number of processors to',num_processors) 
      except:
        pass

    # We assume we are using a focal plane array and want to compute the TEC/RM
    # needed to correct observations in each of the beams
    # do ionosphere predictions in parallel
    numi=Iono_agw.get_Num_Ionospheric_Predictions()
    print ('*** number of predictions: ', numi)
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
    
    print ('     ',file=log)
    print ('ALBUS report column explanation (zero relative):',file=log)
    print ('0 - sequence number (zero relative)',file=log)
    print ('1 - separator colon',file=log)
    print ('2 - value of 0 - valid data/calculation',file=log)
    print ('    value of 1 - invalid data/calculation (usually, but not always, because elevation < 0 )',file=log)
    print ('3 - relative time in seconds relative to reference time',file=log)
    print ('4 - time step between sequence calculations (default is 300 sec)',file=log)
    print ('5 - elevation in degrees',file=log)
    print ('6 - azimuth in degrees',file=log)
    print ('7 - TEC (in tec units) in the current azimuth/elevation direction',file=log)
    print ('8 - Rotation Measure (radians/m^2) in the current azimuth/elevation direction',file=log)
    print ('9 - correction factor to convert STEC to value at the zenith',file=log)
    print ('10 - formal error in STEC fit (personally I find this error to be too ',file=log)
    print ('     large but I am not sure how to adjust it)',file=log)
    print ('     ',file=log)


# read in positions from data file
    max_j = 1
    try:
      positions_ascii  = get_askap_beam_locations(positions_file)
      if len(positions_ascii) > 0:
        max_j = len(positions_ascii)
    except:
        pass
    # 
    for j in range(max_j):
      location_ra = []
      location_dec = []
      if max_j == 1: 
        if Ra == 0 and Dec == 0 and len(MS) > 0: 
          direction = get_observing_position(MS)
          if direction[0] < 0.0:
            direction_0 = 2 * math.pi - direction[0]
          else:
            direction_0 = direction[0]
          print ('observation direction ', math.degrees(direction_0), math.degrees(direction[1]), file=log)
          retval = Iono_agw.set_source_position(direction[0],direction[1])
 
        else:
          try:
            if El < 0.0:
              print('observation direction ', Ra, Dec, file=log)
              (Ra,Dec)=ac.radec_str_to_rad2(Ra,Dec)
            else:
              print ('observation in fixed Az El direction ', Az, El, file=log)
          except:
            if El < 0.0:
              print('observation direction ', Ra, Dec, file=log)
              (Ra,Dec)=ac.radec_str_to_rad2(Ra,Dec)
            else:
              print ('observation in fixed Az El direction ', Az, El, file=log)
          retval = Iono_agw.set_source_position(Ra, Dec)

      else:
        try:
          (obs_ra, obs_dec)  = ac.radec_str_to_rad2(positions_ascii[j][0], positions_ascii[j][1])
          print ('positions_ascii ra dec ', obs_ra, obs_dec)
        except:
          pass
      location_ra.append(obs_ra)
      location_dec.append(obs_dec)

      print ('  ',file=log)
      print ('#######################################################',file=log)
      print ('observation:', j,' direction ', positions_ascii[j][0],positions_ascii[j][1],file=log)
      print ('  ',file=log)
      print ('observation:', j,' direction ', positions_ascii[j][0],positions_ascii[j][1])

      TASKS = [(predict_iono_corrections, (i, times, location_ra, location_dec)) for i in range(NUMBER_OF_PROCESSES)]
      print ('number of prediction tasks ', len(TASKS))
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
      print ('****** finished ionosphere predictions for ra and dec', math.degrees(location_ra[0]), math.degrees(location_dec[0]), '*******************')

     # report results
      print ('seq  rel_time time_width El         Az         STEC           RM (rad/m2)   VTEC factor   STEC_error',file=log)
      for i in range(NUMBER_OF_PROCESSES):
       data_list = report_list[i]
       num_to_report = len(data_list)
       for k in range(1,num_to_report):
         data = data_list[k]
         print (data[0],':',data[1],data[2], data[3], math.degrees(data[4]), math.degrees(data[5]),data[6], data[7], data[8], data[9],file=log)

      print (' ',file=log)
    print ('*********** finished task!! ***************')

# compute total run time
    stoptime = time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
    print ("Stop at %s" % stoptime)
    process_end = time.time()
    duration = (process_end - process_start)/60.0
    print ('process_ionosphere: total run time: %7.2f minutes' % duration)
    print ('process_ionosphere: total run time: %7.2f minutes' % duration,file=log)

if __name__ == "__main__":

# position below is that of MRO1 GPS station
  LONG="116:38:15.0"
  LAT="-26:41:47.9"
  HEIGHT=354.1

# position below is that of ASKAP 'mean' position from RMextract
  LAT="-26:41:45.78"
  LONG= "116:37:54.67"
  HEIGHT= 369.8427133448422

  RA="22:10:35.412"
  DEC="-44:49:50.70"
  START_TIME="2016/08/11 11:01:06.2"
  END_TIME="2016/08/11 23:01:13.4"
  START_TIME = "2014/12/15 10:27:27.0"
  END_TIME = "2014/12/15 21:29:57.0"
  OBJECT="NGC7232-12-multi_dirs_iono"
  process_ionosphere(Ra=RA,Dec=DEC,Lat=LAT,Long=LONG,Height=HEIGHT,start_time=START_TIME,end_time=END_TIME,processing_option="RI_G03",do_serial=0,num_processors=8, gps_data_directory="/home/twillis/twillis1/ASKAP_test_data_2014_dec",object=OBJECT,positions_file = './NGC7232_beams')
