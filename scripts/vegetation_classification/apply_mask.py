import cv2
import numpy as np

mask = cv2.imread("data_source/mrd_085_eos_vis_20151010_1205_maskd.png")[:,:,0]
mask2 = cv2.imread("data_source/aligned/2012/IMG_8748.png")[:,:,0]
mask[mask2 == 0] = 0

for year in ["2012", "2015", "2021"]:
    pred = np.load(f"results/{year}.npy")
    pred[mask == 0] = 0
    np.save(f"results/{year}_masked.npy", pred)
    
    pred = cv2.imread(f"results/{year}.png")
    pred[mask == 0] = 0
    cv2.imwrite(f"results/{year}_masked.png", pred)