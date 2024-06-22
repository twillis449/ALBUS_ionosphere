from multiprocessing import Process, Queue
from shapely.geometry import Polygon, MultiPolygon, Point
from scipy.spatial import distance
import json
import math
import numpy as np
import collections

def worker(input, output):
    for func, args in iter(input.get, 'STOP'):
        result = func(*args)
        output.put(result)


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
    y0 = math.fabs((p1[1] - p2[1]) * scale)
    y0 = (p1[1] - p2[1]) * scale
    tan = math.degrees(math.atan2(x0,y0)) 
    dist = math.sqrt(x0 * x0 + y0 * y0)
    return x0 * x0 + y0 * y0 , tan

# Function to find the maximum
# distance between any two points
def maxDist(p,scale):
    n = len(p)
#   print ('length of p ', n)
    maxm = 0
    tan = 0
    p_0 = p[0]
    p_1 = p[1]
    # Iterate over all possible pairs
    for i in range(n):
        for j in range(i + 1, n):
            # Update maxm
            dist, tan1 = simple_distance(p[i], p[j], scale)
            if dist > maxm:
              maxm = dist
              tan = tan1
              p_0 = p[i]
              p_1 = p[j]
    tan = 90.0 + tan
    if tan < 0.0:
      tan = tan + 180.0
    if tan > 180.0:
      tan = tan - 180.0
    result = (math.sqrt(maxm), tan)
    print('maxDist result',result)
    return (math.sqrt(maxm), tan, p_0, p_1)

def process_json_file(in_data, pixel_size=0.0):

  num_contours = in_data['num_contours']
  try:
    coords = in_data['coords']
  except:
    coords = []
  if len(coords) == 0 and in_data['manual'] == False:
    polygon_list = []
    return polygon_list, coords
# print('number of separate contours', num_contours)
  outer_list = []
  for j in range(num_contours):
    if in_data.get(str(j)):
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
#   print('**** coords', coords)
    if in_data['manual']:
      n = 1
    else:
      n = len(coords)
    polygon_list = []
    las = []
    las_pa = []
    p_max = []
    for i in range(n):
#     print('outer iteration', i)
      found = False 
#     print('coordinate ', coords[i])
      if not in_data['manual']:
        Pt = Point(coords[i])
      else: Pt = Point([0.0,0.0])
      for l in range(length):
#       print('inner iteration', l)
        rslt = sortedArr[l] 
        contour_number = int(rslt[0])
        poly_coord = in_data[str(contour_number)]
        p = Polygon(poly_coord)
        if len(p.exterior.coords) > 500:
          old = len(p.exterior.coords)
          p = p.simplify(0.5, True)
          new = len(p.exterior.coords)
          if new < old:
            in_data[str(contour_number)] = list(p.exterior.coords)
        if p.contains(Pt) or in_data['manual'] :
#         print('********* containing contour', l)
          polygon_list.append(p)
          if pixel_size > 0.0:
#           result = maxDist1(poly_coord,pixel_size)
#           print('maxDist1 gives ', result)
#           las.append(result)
            result = maxDist(poly_coord,pixel_size)
            las.append(result[0])
            las_pa.append(result[1])
            p_max.append(result[2])
            p_max.append(result[3])
    if pixel_size > 0:  
      result = maxDist(p_max,pixel_size)
      max_las = result[0] 
      max_pa = result[1]
      print('Source has maximum angular size and position angle', max_las, max_pa)
      print('las, las_pa', las,las_pa)
      return polygon_list, coords, max_las, las, max_pa, las_pa
    else:
      return polygon_list, coords

def get_contained_points(y, xmin, xmax,i, p): 
     x_list = []
     y_list = []
     for x in range(xmin,xmax):
       pt = Point(x,y)
# Testing for contained points can be somewhat slow if you have
# a big polygon containing many points
# Note that we flip x and y coordinates going from matplotlib
# display coordinates to numpy array coordinates
       if p.contains(pt):
           x_list.append(y)
           y_list.append(x)
     if len(x_list) != len(y_list):
#      print('error x_list and y_list have different lengths',  len(x_list), len(y_list))
       return (-1, -1, -1.0,-1.0 )
     else:
       if len(x_list) > 0 and len(y_list) > 0:
         return (i, y, (x_list, y_list))
       else:
         return (-1, -1, -1.0,-1.0 )

def get_interior_locations(polygon_list):
#   print('Getting points interior to polygon boundaries ...')
    num_processors = 1
#   if num_processors <= 2 and len(polygon_list) > 1:
    if num_processors <= 2:
      try:
        import multiprocessing
        processors =  multiprocessing.cpu_count()
        if processors > num_processors:
          num_processors = processors
      except:
        pass
#   print ('*** setting final number of processors to',num_processors)
    TASKS = []
#   print('getting pixels inside polygon, working ...')
    for i in range(len(polygon_list)):
      p = polygon_list[i]
# Can we simplify the polygon?
      if len(p.exterior.coords) > 500:
        old = len(p.exterior.coords)
        p = p.simplify(0.5, True)
        new = len(p.exterior.coords)
        if new < old:
#         print('simplify shrank polygon points from ', old, ' to ', new)
          polygon_list[i] = p
      (minx, miny, maxx, maxy) = p.bounds

# set up parallelprocessing
      for y in range(int(miny-1), int(maxy+1)):
        TASKS.append((get_contained_points, (y, int(minx-1), int(maxx+1), i, p)))
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

    interior_polygons = {}
    result_sum = 0
    first = True 
    for i in range(len(polygon_list)):
       interior_polygons[i] = ([], [])

# get output from tasks
    for i in range(len(TASKS)):
        result = done_queue.get(timeout=2000)
        if result[0] >= 0:
          data = result[2]
          if len(data[0]) != len(data[1]):
            print('retrieved data have different lengths!')
          else:
             data = interior_polygons[result[0]]
             x_list = data[0]
             y_list = data[1]
             x_list = x_list + result[2][0]
             y_list = y_list + result[2][1]
             interior_polygons[result[0]] = (x_list, y_list)

# make sure  things shut down
    for i in range(num_processors):
      task_queue.put('STOP')

    interior_points = []
    for i in range(len(polygon_list)):
      data = interior_polygons[i]
      x_list = data[0]
      y_list = data[1]
      interior_points.append((i,(np.array(x_list), np.array(y_list))))
    return interior_points


