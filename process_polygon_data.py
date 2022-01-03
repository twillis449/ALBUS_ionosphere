from multiprocessing import Process, Queue
from shapely.geometry import Polygon, MultiPolygon, Point
from scipy.spatial import distance
import json
import math
import numpy as np

def worker(input, output):
    for func, args in iter(input.get, 'STOP'):
        result = func(*args)
        output.put(result)

def get_interior_points(i, p):
  print('in get interior points')
  print('processing polygon', i)
  points = []
  (minx, miny, maxx, maxy) = p.bounds
  for y in range(int(miny-1), int(maxy+1)):
     for x in range(int(minx-1), int(maxx+1)):
       pt = Point(x,y)
       if p.contains(pt):
           points.append((x,y))
# we want the polygon boundary points  to be included
# --- not at present
# boundary = list(p.exterior.coords)
# last = len(boundary)
# boundary = boundary[:last-1]  # last member == first (added by shapely)
# print('polygon boundary', boundary)
# print('polygon points', points)
# all_points = points + boundary 
  all_points = points 
  print('number of points in polygon ', i, len(all_points))
  return (i, all_points)

def maxDist1(p,scale):
   coords =  np.array(p) * scale
   array = distance.cdist(coords, coords, 'euclidean')
   shape = array.shape
   max = []
   for i in range(shape[0]):
      max.append(np.max(array[i]))
   max_of_max = np.max(max)
   return max_of_max

def simple_distance(p1,p2,scale):
    x0 = (p1[0] - p2[0]) * scale
    y0 = (p1[1] - p2[1]) * scale
    try:
      tan = math.degrees(math.atan2(x0,y0)) 
    except:
      tan = 0.0
    dist = math.sqrt(x0 * x0 + y0 * y0)
#   print('delta x y, dist, tan angle', x0, y0, dist, tan)
    return x0 * x0 + y0 * y0 , tan

# Function to find the maximum
# distance between any two points
def maxDist(p,scale):
    n = len(p)
#   print('number of polygons', n)
    maxm = 0
    tan = 0
    # Iterate over all possible pairs
    for i in range(n):
        for j in range(i + 1, n):
            # Update maxm
            dist, tan1 = simple_distance(p[i], p[j], scale)
            if dist > maxm:
              maxm = dist
              tan = tan1
#             print('updated tan angle', tan)
    if tan < 0:
      tan = abs(tan)
    else:
      tan = 180 - tan
    return math.sqrt(maxm), tan

def process_json_file(filename, pixel_size=0.0):
  print('calc parms processing json file', filename)

  with open(filename) as f:
    in_data = json.load(f)
  num_contours = in_data['num_contours']
  coords = in_data['coords']
# print('number of separate contours', num_contours)
  outer_list = []
  for j in range(num_contours):
    poly_coord = in_data[str(j)]
    if len(poly_coord) > 2:
      p = Polygon(poly_coord)
      inner_list = [j,p.area]
      outer_list.append(inner_list)
  length = len(outer_list)
  if length > 0:
    arr2d = np.array(outer_list)
    columnIndex = 1
    sortedArr = arr2d[arr2d[:,columnIndex].argsort()[::-1]]     # sorts in ascending order
#   print('sorted arr2d', sortedArr)
      
#   find n largest values
    print('**** coords', coords)
    n = len(coords)
    polygon_list = []
    las = []
    las_pa = []
    poly_coord_all = []
    for i in range(n):
      found = False 
      print('coordinate ', coords[i])
      Pt = Point(coords[i])
      for l in range(length):
        rslt = sortedArr[l] 
        contour_number = int(rslt[0])
        poly_coord = in_data[str(contour_number)]
        p = Polygon(poly_coord)
        if p.contains(Pt):
          poly_coord_list = []
          print('********* containing contour', l)
          found = True
          for j in range(len(poly_coord)):
            a = poly_coord[j][0]
            b = poly_coord[j][1]
            poly_coord_all.append((a,b))
          polygon_list.append(p)
          if pixel_size > 0.0:
            ang_size = maxDist1(poly_coord,pixel_size)
            print('cdist angular size', ang_size)
            ang_size, pa = maxDist(poly_coord,pixel_size)
            print('maxDist angular size and position angle', ang_size, pa)
            las.append(ang_size)
            las_pa.append(pa)
    if not found:
      print('no polygon containing',coords[i])
    if pixel_size > 0:  
      max_las = maxDist1(poly_coord_all, pixel_size)
      print('cdist max_las', max_las)
      max_las, max_pa = maxDist(poly_coord_all,pixel_size)
      print('maxDist max_las size and position angle', max_las, max_pa)
      return polygon_list, coords, max_las, las, max_pa, las_pa
    else:
      return polygon_list, coords


def get_interior_locations(polygon_list):
    num_processors = 1
    if num_processors <= 2 and len(polygon_list) > 1:
      try:
        import multiprocessing
        processors =  multiprocessing.cpu_count()
        if processors > num_processors:
          num_processors = processors
#         print ('*** setting number of processors to',num_processors)
      except:
        pass
    print ('*** setting final number of processors to',num_processors)
    TASKS = []
    for i in range(len(polygon_list)):
      TASKS.append((get_interior_points, (i,polygon_list[i])))

    NUMBER_OF_PROCESSES = num_processors
   # Create queues
    task_queue = Queue()
    done_queue = Queue()

  # Submit tasks
    for task in TASKS:
      task_queue.put(task)

    # Start worker processes
    for i in range(NUMBER_OF_PROCESSES):
      Process(target=worker, args=(task_queue, done_queue)).start()

    interior_points = []
    result_sum = 0
    for i in range(len(TASKS)):
      try:
        result = done_queue.get(timeout=2000)
#       print('caught polygon', result[0])
        interior_points.append(result)
        result_sum = result_sum + 1
      except:
        pass
    print('number of interior point lists', len(interior_points))

# Tell child processes to stop
    for i in range(NUMBER_OF_PROCESSES):
        task_queue.put('STOP')
    print ('*********** finished collecting polygon points ! ******* \n')
    return interior_points


