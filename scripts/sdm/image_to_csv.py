import numpy as np
import pandas as pd
from glob import glob

def image_to_df(array, chnames):
    uv = np.meshgrid(np.arange(0,array.shape[1]), np.arange(0,array.shape[0]))
    uv = np.stack(uv, axis = 2)
    concat = np.concatenate([uv, array], 2).reshape(-1, 2+array.shape[2])
    columns = ["u", "v"]
    columns.extend(chnames)
    df = pd.DataFrame(concat, columns=columns)
    df[["u", "v"]] = df[["u", "v"]].astype("int16")
    return df

# in_paths = ["results/diff_haimatsu_5x5.npy", "results/diff_sasa_5x5.npy", "results/2012_5x5.npy", "results/2021_5x5.npy"]
#in_paths = ["results/use_this/2012_5x5.npy", "results/use_this/2021_5x5.npy"]
#out_paths = ["ortho/data/2012_5x5.csv", "ortho/data/2021_5x5.csv"]

# for in_path, out_path in zip(in_paths, out_paths):
#     array = np.load(in_path)
#     array = array[:,:,np.newaxis]
#     df = image_to_df(array, chnames = ["data"])
#     df.to_csv(out_path, index=False)

from glob import glob
import cv2
in_paths = glob("ortho/data/snow/aligned/*.png")
out_paths = [in_path.replace(".png", ".csv") for in_path in in_paths]

for in_path, out_path in zip(in_paths, out_paths):
    array = cv2.imread(in_path, cv2.IMREAD_GRAYSCALE)
    array = array[:,:,np.newaxis]
    df = image_to_df(array, chnames = ["snowmelt_doy"])
    df.to_csv(out_path, index=False)