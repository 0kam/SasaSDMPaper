import cv2
import numpy as np
from glob import glob
import os
import pandas as pd
from tqdm import tqdm
from scipy import optimize as opt
import copy

def homography_lensdist(src_img, dst_img, flann = True, ransac_th=100):
  im1 = src_img
  im2 = dst_img
  # Akaze descripter
  akaze = cv2.AKAZE_create()
  kp1, des1 = akaze.detectAndCompute(im1, None)
  kp2, des2 = akaze.detectAndCompute(im2, None)
  if flann:
      # Flann matcher
      FLANN_INDEX_LSH = 6
      index_params= dict(algorithm = FLANN_INDEX_LSH,
                         table_number = 6,  
                         key_size = 12,     
                         multi_probe_level = 1) 
      search_params = dict(checks = 50)
      matcher = cv2.FlannBasedMatcher(index_params, search_params)
  else: 
      # Brute Force matcher
      matcher = cv2.BFMatcher()
  matches = matcher.knnMatch(des1, des2, k = 2)
  ratio = 1
  good = []
  for m, n in matches:
        if m.distance < ratio * n.distance:
            good.append(m)
  pts1 = np.float32([ kp1[match.queryIdx].pt for match in good ])
  pts2 = np.float32([ kp2[match.trainIdx].pt for match in good ])
  pts1 = pts1.reshape(-1,1,2)
  pts2 = pts2.reshape(-1,1,2)
  
  # Filter matched points with RANSAC 
  hm, mask = cv2.findHomography(pts1, pts2, cv2.RANSAC, ransac_th)

  pts1 = pts1[mask.astype('bool')]
  pts2 = pts2[mask.astype('bool')]
  
  # Lens Distortion
  def distort(pts, params):
    k1 = params[0]
    k2 = params[1]
    k3 = params[2]
    p1 = params[3]
    p2 = params[4]
    k4 = params[5]
    k5 = params[6]
    k6 = params[7]
    s1 = 0 #params[8]
    s2 = 0 #params[9]
    s3 = 0 #params[10]
    s4 = 0 #params[11]
    w = im1.shape[1]
    h = im1.shape[0]
    centre = np.array([(w - 1) / 2, (h - 1) / 2], dtype = 'float32')
    x1 = (pts[:,0] - centre[0]) / centre[0]
    y1 = (h / w) * (pts[:,1] - centre[1]) / centre[1]
    r = (x1**2 + y1**2)**0.5
    r2 = r**2
    r4 = r**4
    r6 = r**6
    
    x1_d = x1 * (1 + k1*r2 + k2*r4 + k3*r6)/(1 + k4*r2 + k5*r4 + k6*r6) + 2*p1*x1*y1 + p2*(r2*2*x1**2) + s1*r2 + s2*r4
    y1_d = y1 * (1 + k1*r2 + k2*r4 + k3*r6)/(1 + k4*r2 + k5*r4 + k6*r6) + 2*p2*x1*y1 + p1*(r2*2*y1**2) + s3*r2 + s4*r4
    x1_d = x1_d * centre[0] + centre[0]
    y1_d = (w / h) * y1_d * centre[1] + centre[1]
    pts_d = np.stack([x1_d, y1_d], axis = 0).T
    return pts_d
  
  def homography(pts1, pts2):
    pts1 = pts1.reshape(-1,1,2)
    pts2 = pts2.reshape(-1,1,2)
    hmat, mask = cv2.findHomography(pts1, pts2)
    pts1 = pts1.reshape(-1,2)
    pts2 = pts2.reshape(-1,2)

    pts1 = np.insert(pts1, 2, 1, axis = 1)
    pts1 = np.dot(hmat, pts1.T).T
    pts1[:,0] = pts1[:,0] / pts1[:,2]
    pts1[:,1] = pts1[:,1] / pts1[:,2]
    pts1 = pts1[:,0:2]
    rmse = np.mean(((pts1[:,0] - pts2[:,0])**2 + (pts1[:,1] - pts2[:,1])**2)**0.5) 

    return pts1, rmse, hmat
  
  # Lens Distortion Correction -> Homography Transformation 
  def distort_rmse(params):
    pts1_d = distort(pts1, params)
    pts1_dh = homography(pts1_d, pts2)
    return pts1_dh[1]
  
  # Minimize the rmse of matched points
  res = opt.minimize(distort_rmse, x0 = [0] * 8, method = 'Nelder-Mead')

  # Make map and remap source image
  height, width, channels = im1.shape
  map_x, map_y  = np.meshgrid(np.arange(width), np.arange(height))
  grid = np.stack([map_x.flatten(), map_y.flatten()]).T
  p = copy.copy(res.x)
  p = -p
  grid_d = distort(grid, p)
  map_d = grid_d.T.reshape([2, height, width]).astype('float32')
  im1_d = cv2.remap(im1, map_d[0,:,:], map_d[1,:,:], interpolation = cv2.INTER_LINEAR)
  pts1_d = distort(pts1, res.x)
  pts1_dh, rmse, hmat = homography(pts1_d, pts2)
  
  im1_dh = cv2.warpPerspective(im1_d, hmat, (im2.shape[1], im2.shape[0]))
  
  # Return the transformed source image, map_u, map_v, homography matrix
  return im1_dh, map_d[0,:,:], map_d[1,:,:], hmat, rmse, len(pts1)

def homography(src_img, dst_img, flann = True, ransac_th=5):
    im1 = src_img
    im2 = dst_img
    # Akaze descripter
    akaze = cv2.AKAZE_create()
    kp1, des1 = akaze.detectAndCompute(im1, None)
    kp2, des2 = akaze.detectAndCompute(im2, None)
    if flann:
        # Flann matcher
        FLANN_INDEX_LSH = 6
        index_params= dict(algorithm = FLANN_INDEX_LSH,
                          table_number = 6,  
                          key_size = 12,     
                          multi_probe_level = 1) 
        search_params = dict(checks = 50)
        matcher = cv2.FlannBasedMatcher(index_params, search_params)
    else: 
        # Brute Force matcher
        matcher = cv2.BFMatcher()
    matches = matcher.knnMatch(des1, des2, k = 2)
    ratio = 0.75
    good = []
    # Filter matching points
    for m, n in matches:
        if m.distance < ratio * n.distance:
            good.append(m)

    pts1 = np.float32([ kp1[match.queryIdx].pt for match in good ])
    pts2 = np.float32([ kp2[match.trainIdx].pt for match in good ])
    pts1 = pts1.reshape(-1,1,2)
    pts2 = pts2.reshape(-1,1,2)

    # Find homography matrix
    hmat, mask = cv2.findHomography(pts1, pts2, cv2.RANSAC, ransac_th)
    pts1 = pts1[mask.astype('bool')]
    pts2 = pts2[mask.astype('bool')]
    
    # Culculate Root Mean Square Error
    pts1 = pts1.reshape(-1,2)
    pts2 = pts2.reshape(-1,2)
    pts1 = np.insert(pts1, 2, 1, axis = 1)
    pts1 = np.dot(hmat, pts1.T).T
    pts1[:,0] = pts1[:,0] / pts1[:,2]
    pts1[:,1] = pts1[:,1] / pts1[:,2]
    pts1 = pts1[:,0:2]
    rmse = np.mean(((pts1[:,0] - pts2[:,0])**2 + (pts1[:,1] - pts2[:,1])**2)**0.5) 
    
    # Homography Transformation 
    im1_h = cv2.warpPerspective(im1, hmat, (im2.shape[1], im2.shape[0]))

    # Return the transformed source image, homography matrix, rmse, and number of matched points

    return im1_h, hmat, rmse, len(pts1)

# Creating output directories if not found
for d in ["data_source/aligned_composite/2012", "data_source/aligned_composite/2021"]:
    if os.path.exists(d) == False:
        os.makedirs(d)


# Applying alignment
results = []
#target = "data_source/mrd_085_eos_vis_20151010_1205.png"
for d in ["data_source/source/2012", "data_source/source/2021"]:
    sources = sorted(glob(d+"/*.JPG"))
    targets = sorted(glob("data_source/aligned/2015/*"))
    for s, t in zip(sources, targets):
    #for s in sources:
        src_img = cv2.imread(s)
        dst_img = cv2.imread(t)
        src_h, map_u, map_v, hmat, rmse, n_matches = \
            homography_lensdist(src_img, dst_img, ransac_th=5)
        res = dict(
            source = s,
            destination = t,
            rmse = rmse,
            n_matches = n_matches
        )
        res = pd.DataFrame.from_dict(res, orient="index").T
        results.append(res)
        print("-----")
        print(res)
        print("-----")
        cv2.imwrite(s.replace("/source/", "/aligned/").replace(".JPG", ".png"), src_h)

sources = [
    "data_source/source/2012/IMG_8748.JPG",
    "data_source/source/2012/IMG_8956.JPG",
    "data_source/source/2012/IMG_9164.JPG",
    "data_source/source/2012/IMG_9304.JPG",
    "data_source/source/2012/IMG_9514.JPG",
    "data_source/source/2021/IMG_8081.JPG",
    "data_source/source/2021/IMG_8298.JPG"
]

targets = [
    "data_source/aligned/2012/IMG_8819.png",
    "data_source/aligned/2012/IMG_9038.png",
    "data_source/aligned/2012/IMG_9038.png",
    "data_source/aligned/2012/IMG_9038.png",
    "data_source/aligned/2012/IMG_9038.png",
    "data_source/aligned/2021/IMG_8172.png",
    "data_source/aligned/2021/IMG_8172.png"
]

for s, t in zip(sources, targets):
    src_img = cv2.imread(s)
    dst_img = cv2.imread(t)
    src_h, map_u, map_v, hmat, rmse, n_matches = \
        homography_lensdist(src_img, dst_img, ransac_th=5)
    res = dict(
        source = s,
        destination = t,
        rmse = rmse,
        n_matches = n_matches
    )
    res = pd.DataFrame.from_dict(res, orient="index").T
    results.append(res)
    print("-----")
    print(res)
    print("-----")
    cv2.imwrite(s.replace("/source/", "/aligned/").replace(".JPG", ".png"), src_h)