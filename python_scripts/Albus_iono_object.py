import AlbusIonosphere

class AlbusIono:
  def __init__(self, len_GPS_receivers=0):
    print('in AlbusIono __init')
    self.iono = AlbusIonosphere
    self.clear_everything()
    if len_GPS_receivers > 0:
      self.cal_observations_init(len_GPS_receivers)

  def clear_everything(self):
    print('in AlbusIono clear_everything')
    if(self.iono.clear_everything()):
      raise RuntimeError("could not clear AlbusIonosphere")

  def cal_observations_init(self,len_GPS_receivers=0):
    print('in AlbusIono cal_observations_init')
    if len_GPS_receivers > 0:
      if(self.iono.cal_observations_init(len_GPS_receivers)):
        raise RuntimeError("could not init self.iono")

  def cal_observations_set_cal_obs(self,GPS0, GPS1, sh0, sh1, Sat_small, \
      block_small, STEC, sigma, sta_bias_valid):
    print('in AlbusIono cal_observations_set_cal_obs')
    if(self.iono.cal_observations_set_cal_obs(
          GPS0, GPS1, sh0, sh1, Sat_small, block_small,
          STEC, sigma, sta_bias_valid)):
      raise RuntimeError("could not set receiver data for self.iono")

  def cal_observations_set_parameters(self, \
     fit_type, Max_Sat_Sky_Angle,Min_Sat_Elev,Max_Rec_Dist_From_Tele, \
     Max_Iono_Pierce_Dist,Default_Iono_Height,Averaging_Time_Half_Width, \
     Num_Ionosphere_Parameters, Num_Ionosphere_Heights, Num_Time_Terms, \
     Theo_Model_Type, Bias_Fit_Type):

    print('in AlbusIono cal_observations_set_parameters')
    if self.iono.cal_observations_set_parameters(fit_type, \
      Max_Sat_Sky_Angle,Min_Sat_Elev,Max_Rec_Dist_From_Tele, \
      Max_Iono_Pierce_Dist,Default_Iono_Height,Averaging_Time_Half_Width, \
      Num_Ionosphere_Parameters, Num_Ionosphere_Heights, Num_Time_Terms, \
      Theo_Model_Type, Bias_Fit_Type):
        raise RuntimeError("could not set criteria for self.iono")

  def cal_observations_set_times(self, len_MJD_array, MJD_array): 
    print('in AlbusIono cal_observations_set_times')
    if(self.iono.cal_observations_set_times(len_MJD_array, MJD_array)):
      raise RuntimeError("could not set MJDs for self.iono")
    print('finished in AlbusIono cal_observations_set_times')


  def cal_observations_set_sat_pos(self, sh0, sh1, sh2, sat_XYZ):
    print('in AlbusIono cal_observations_set_sat_pos')
    if(self.iono.cal_observations_set_sat_pos(sh0, sh1, sh2, sat_XYZ)):
      raise RuntimeError("could not set sat pos for self.iono")

  def cal_observations_init2(self):
    print('in AlbusIono cal_observations_init2')
    if(self.iono.cal_observations_init2()):
      raise RuntimeError("could not correct bias levels in self.iono")

  def set_ionosphere_IRI(self,tolerance_low,tolerance_high):
    print('in AlbusIono set_ionosphere_IRI')
    retval = self.iono.set_ionosphere_IRI(tolerance_low,tolerance_high)

  def set_ionosphere_PIM(self,tolerance_low,tolerance_high):
    retval = self.iono.set_ionosphere_PIM(tolerance_low,tolerance_high)

  def set_ionosphere_GPS(self,tolerance_low,tolerance_high):
    print('in AlbusIono set_ionosphere_GPS')
    retval = self.iono.set_ionosphere_GPS(tolerance_low,tolerance_high)


  def set_reference_time(self,year_ref,month_ref, day_ref,hr_ref,min_ref,sec_ref):
    print('in AlbusIono set_reference_time')
    retval = self.iono.set_reference_time(year_ref,
                                                month_ref,
                                                day_ref,
                                                hr_ref,
                                                min_ref,
                                                sec_ref)
    if(retval < 0):
      raise RuntimeError("Error: self.iono.set_reference_time gave %d"%retval)
    return retval


  def set_source_position(self, direction0, direction1):
    print('in AlbusIono set_source_position')
    retval=self.iono.set_source_position(direction0,direction1)
    return retval

  def set_time_step(self, delta_time):
    print('in AlbusIono set_time_step')
    retval=self.iono.set_time_step(delta_time)
    return retval

  def set_station_position(self, station_pos0,station_pos1,station_pos2):
    print('in AlbusIono set_station_position')
    retval = self.iono.set_station_position(station_pos0,station_pos1,station_pos2)
    return retval

  def set_scan_times(self,begin_time, num_seconds):
    print('in AlbusIono set_scan_times')
    self.iono.set_scan_times(begin_time, num_seconds) 

  def get_ionospheric_prediction(self,seq_number):
    print('in AlbusIono get_ionospheric_prediction')
    retval,pred_time, time_width, El, Az, STEC, SRM, VTEC_factor, STEC_ERR = \
        self.iono.get_ionospheric_prediction(seq_number)
    if(retval < 0):
       raise RuntimeError("Error: self.iono.get_ionospheric_prediction gave %d"%retval)
    return retval,pred_time, time_width, El, Az, STEC, SRM, VTEC_factor, STEC_ERR

  def get_TAI_UTC(self,ymdhms0, ymdhms1,ymdhms2):
    print('in AlbusIono get_TAI_UTC')
    retval, TAI_UTC = self.iono.get_TAI_UTC(ymdhms0,ymdhms1,ymdhms2, 0.5)
    return retval, TAI_UTC

  def get_Num_Ionospheric_Predictions(self):
    print('in AlbusIono get_Num_Ionospheric_Predictions')
    return self.iono.get_Num_Ionospheric_Predictions()
 
  def __copy__(self):
    print('in __copy__')
#   cls = self.__class__
#   result = cls.__new__(cls)
#   result.__dict__.update(self.__dict__)
#   return result
#   newone = type(self)()
#   newone .__dict__.update(self.__dict__)
#   return newone

    return self

  def __deepcopy__(self, memo):
    print('in __deepcopy__')
#   cls = self.__class__
#   result = cls.__new__(cls)
#   memo[id(self)] = result
#   for k,v in self.__dict__.items():
#     setattr(result, k, deepcopy(v, memo))
#   return result

    return self
