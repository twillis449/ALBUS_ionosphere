# a function to read and process a 'pipeline' file giving object names, along with positions
# and other information. This script has been used to process various types of files so things
# have varied over time

def process_input_file(filename,return_morphology_parameters=False):
    print('filename is', filename)
    print('return_morphology_parameters', return_morphology_parameters)
    text = open(filename, 'r').readlines()
    print('text is ', text)
    info = text[0].split()
    print('opening info ', info)
    freq = info[1]

    names = []
    ra_deg= []
    dec_deg = []
    las = []
    las_raw = []
    red_shift = []
    spec_index = []
        
    L = len(text)
#   skip over all stuff before actual data
    for i in range(1,L):
      print('i, text[i]', i, text[i])
      if text[i][0] != '#':  # skip this object
        info = text[i].split()
        print('info', info)
        names.append(info[0])
        if len(info) > 20:
          index = 3
        else:
          index = 1
          try:
            test = float(info[index])
          except:
            index = 2
        ra_deg.append(info[index])
        dec_deg.append(info[index+1])
        las_raw.append(info[index+2])
        angle = info[index+2]
        red_shift.append(info[index+3])
        try: 
          spec_index.append(info[index+4])
          print('using spectral index of ', spec_index)
        except:
          print('using default spectral index')
          spec_index.append('-0.75')
        try:
          ang_size = str(int(2.0*float(angle) + 1.0))
          las.append(ang_size)
          print('appending angle')
        except:
          las.append('5')
          print('appending default angle')
    print('-------------------- finished reading input\n')
    if return_morphology_parameters:
       return freq, names, ra_deg, dec_deg
    else:
       return freq, names, ra_deg, dec_deg, las, las_raw, red_shift, spec_index
