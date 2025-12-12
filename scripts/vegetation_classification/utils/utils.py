from glob import glob
from PIL import Image
import torch
from torch.nn import functional as F
from torch.utils.data import Dataset
from torchvision.datasets import DatasetFolder
from torchvision.transforms.functional import to_tensor
import pandas as pd
import json
import cv2
import numpy as np
from tqdm import tqdm
import os
from pathlib import Path
import pylab as pl
from matplotlib import pyplot as plt
from shapely import geometry

from matplotlib.font_manager import FontProperties
font_path = "/usr/share/fonts/truetype/migmix/migmix-1p-regular.ttf"
font_prop = FontProperties(fname=font_path)
plt.rcParams["font.family"] = font_prop.get_name()

# Set up patches
def read_sses(label_dir, image_size, label="all"):
    """
    Read multiple json files created with Semantic Segmentation Editor
    https://github.com/Hitachi-Automotive-And-Industry-Lab/semantic-segmentation-editor
    Parameters
    ----------
    label_dir : str
        A path for a directory which stores json files.
    image_size : tuple of int
        (width, height) of the specified image
    label : str or list of str
        list of labels to use. If "all" (default), use all labels
    shrink : int
        shrink size in pixels  
    hmat : np.array
        homography matrix to apply
    mask : np.array
        mask to apply
    Returns
    -------
    image : np.array
        A masked image.
    labels : pd.DataFrame
        
    """
    objects = []
    for p in glob(label_dir + "/*"):
        with open(p) as js:
            obj = json.load(js)["objects"]
            obj = pd.json_normalize(obj)
            objects.append(obj)
    objects = pd.concat(objects)
    max_class_idx = max(objects["classIndex"])
    class_idx = objects["classIndex"].copy()
    class_idx[class_idx == 0] = max_class_idx + 1
    objects["classIndex"] = class_idx
    if label != "all":
        l = objects.label.copy()
        i = objects.classIndex.copy()
        l[objects.label.isin(label) == False] = "その他"
        i[objects.label.isin(label) == False] = 9999 # 9999 = その他
        objects.label = l
        objects.classIndex = i

    polygons = list(objects["polygon"])
    labels = objects["classIndex"]
    image = np.zeros([image_size[1], image_size[0], 3]) # 0 = unlabelled
    for p, l in zip(polygons, labels):
        polygon = []
        for point in p:
            point = list(point.values())
            polygon.append(point)
        polygon = geometry.Polygon(polygon)
        if polygon.geom_type == "MultiPolygon":
            for poly in polygon:
                if len(poly.bounds) != 0:
                    poly = np.array(poly.exterior.xy).T.reshape(-1, 1, 2).astype("int32")
                    image = cv2.fillPoly(image, [poly], color = (l, l, l))
        else:
            if len(polygon.bounds) != 0:
                polygon = np.array(polygon.exterior.xy).T.reshape(-1, 1, 2).astype("int32")
                image = cv2.fillPoly(image, [polygon], color = (l, l, l))
    labels = objects[["classIndex", "label"]].drop_duplicates()
    return image, labels

def set_patches(label_dir, image_dir, out_dir, kernel_size, label="all"):
    """
    Setting up data folders for supervised image segmentation of time-lapse photographs.
    This function makes a directory for each labels, and save patch data as one .npy binaly file.
    Parameters
    ----------
    label_dir : str
        A directory that has json files created with Semantic Segmentation Editor.
    image_dir : str
        A directory that has images for training models.
    out_dir : str
        An output directory. Subdirectories "labelled" and "unlabelled" will be created.
    kernel_size : tuple of int
        A kernel size.
    label : str or list of str
        list of labels to use. If "all" (default), use all labels
    """
    if os.path.exists(out_dir) is False:
        os.makedirs(out_dir)
    image = Image.open(glob(image_dir + "/*")[0])
    data_name = Path(image_dir).stem
    label_image, labels_list = read_sses(label_dir, image.size, label=label)
    label_image = label_image.astype(int)
    kw = int((kernel_size[0] - 1) / 2)
    kh = int((kernel_size[1] - 1) / 2)
    w, h = image.size

    tensors = {}
    for label in labels_list["classIndex"].values:
        tensors[str(label)] = []
        if os.path.exists(out_dir + str(label)) is False:
            os.mkdir(out_dir + str(label))
    images = torch.stack([to_tensor(Image.open(f)) for f in sorted(glob(image_dir + "/*"))], dim = 0)
    for v in tqdm(range(h)):
        for u in range(w):
            label = label_image[v,u][0]
            if label != 0:
                patch = images[:, :, max(0, v-kh):(min(h, v+kh)+1), max(0, u-kw):(min(w, u+kh)+1)]
                patch = F.interpolate(patch, [kernel_size[1], kernel_size[0]])
                patch = torch.reshape(patch, (patch.shape[0], -1))
                patch = patch * 255
                patch = patch.to(torch.uint8)                 
                tensors[str(label)].append(patch)
    for label in labels_list["classIndex"]:
        out_path = out_dir + str(label) + "/" + data_name + ".npy"
        np.save(out_path, np.stack(tensors[str(label)], axis=0))
        tensors[str(label)] = []

def load_npy(path):
    x = np.load(path)
    return x / 255

class TrainDS(Dataset):
    def __init__(self, patch_dir):
        self.dataset = DatasetFolder(patch_dir, load_npy, "npy")
    
    def __len__(self):
        return len(self.dataset)
    
    def __getitem__(self, idx):
        x, y = self.dataset[idx]
        n = x.shape[0]
        y = y * np.ones(n)
        x = x.reshape(n, -1)
        return x, y

class DrawDS(Dataset):
    """
    A Dataset that returns time series patches of images with a given kernel_size.
    Attributes
    ----------
    image_dir : str
        An input data directory that has a set of time-series images. All images must be same size.
    kernel_size : tuple of int
        A tuple (width, height) of the kernel.
    """
    def __init__(self, image_dir, kernel_size):
        self.image_dir = image_dir
        self.kernel_size = kernel_size
        f = glob(self.image_dir + "/*")[0]
        with Image.open(f) as img:
            self.size = img.size
            self.data_length = img.width*img.height
        self.target_images = torch.stack([to_tensor(Image.open(f)) for f in sorted(glob(self.image_dir + "/*"))], dim = 0)
    
    def __len__(self):
        return self.data_length

    def __getitem__(self, idx):
        center = (idx % self.size[0], idx // self.size[0])
        kw = int((self.kernel_size[0]-1) / 2)
        kh = int((self.kernel_size[1]-1) / 2)
        left = max(center[0] - kw, 0)
        upper = max(center[1] - kh, 0)
        right = min(center[0] + kw, self.size[0])
        lower = min(center[1] + kh, self.size[1])
        patch = self.target_images[:,:,upper:lower+1,left:right+1]
        patch = F.interpolate(patch, [self.kernel_size[1], self.kernel_size[0]])
        patch = torch.reshape(patch, (patch.shape[0], -1))
        patch = patch.numpy()
        return patch


# Drawing functions
def draw_teacher(out_path, label_dir, class_to_idx, image_size, label, cmap = "jet"):
    img, labels = read_sses(label_dir, image_size, label=label)
    mask = img.copy()
    for label in labels["classIndex"]:
        l = class_to_idx[str(label)]
        img[mask == label] = l
    array = img[:,:,0]
    plt.imsave(out_path, array, cmap = cmap)
    np.save(out_path.replace(".png", ".npy"), array)
    img = cv2.imread(out_path)
    img[mask == 0] = 0
    cv2.imwrite(out_path, img)
    return array

def draw_legend(out_path, label_dir, class_to_idx, label, cmap = "jet"):
    _, labels = read_sses(label_dir, (9999,9999), label=label)
    labels = labels.sort_values("classIndex")
    for _, row in labels.iterrows():
        index = row[0]
        name = row[1]
        color = cmap(class_to_idx[str(index)])
        pl.plot(0, 0, "-", c = color, label = name, linewidth = 10)
    pl.legend(loc = "center", prop = {"family": "MigMix 1P"})
    pl.savefig(out_path)
    pl.cla()