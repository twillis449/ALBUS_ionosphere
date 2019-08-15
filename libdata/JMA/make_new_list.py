def make_new_GPS_list(filename):

    text = open(filename, 'r').readlines() 
    fout = open(filename+'_wide', 'w')
    try:
        site_id_found = 0
        for i in range(len(text)):
            if(site_id_found == 0):
              outstr = text[i] 
              fout.write(outstr)
              if(text[i][0:8] == "+SITE/ID"):
                site_id_found = 1
            else:
              if(text[i][0:8] == "-SITE/ID"):
                print 'reached end of input stations file'
                outstr = text[i] 
                fout.write(outstr)
                fp.close()
                fout.close()
              print text[i]
              outstr = text[i][0:55] + '  ' + text[i][56:67] + ' ' + text[i][67:75] + '  \n'
              fout.write(outstr)
    except:
      pass

make_new_GPS_list('new_list')

