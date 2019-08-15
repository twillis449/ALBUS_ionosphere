import AlbusIonosphere
AlbusIonosphere.set_ionosphere_IRI(1E-4,1E-3)
AlbusIonosphere.set_reference_time(2005,3,21,0,0,0)
AlbusIonosphere.set_station_position(6378137,0,0)    
AlbusIonosphere.set_source_position(0,0)       
AlbusIonosphere.set_time_step(30)
AlbusIonosphere.set_scan_times(43000,43500)
AlbusIonosphere.get_Num_Ionospheric_Predictions()
AlbusIonosphere.get_ionospheric_prediction(0)    
for i in xrange(AlbusIonosphere.get_Num_Ionospheric_Predictions()):
    result = AlbusIonosphere.get_ionospheric_prediction(i)
    print result
AlbusIonosphere.set_scan_times(20000,23200)
for i in xrange(AlbusIonosphere.get_Num_Ionospheric_Predictions()):
    result = AlbusIonosphere.get_ionospheric_prediction(i)
    print result
