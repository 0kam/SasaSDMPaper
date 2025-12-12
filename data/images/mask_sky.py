import cv2

path = "data_source/source/2015/mrd_085_eos_vis_20150905_1405.JPG"

img = cv2.imread(path)
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
for i in range(gray.shape[1]):
    for j in range(gray.shape[0]):
        if gray[j, i] >= 250:
            gray[j, i] = 0
        else:
            break

img[gray==0,:] = 0
cv2.imwrite("data_source/sky_mask_2015.png", img)