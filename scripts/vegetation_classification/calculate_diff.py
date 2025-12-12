import numpy as np
from matplotlib import pyplot as plt
from matplotlib.colors import ListedColormap

labels = {
    "sasa": 0,
    "others": 1,
    "no_vegetation": 2,
    "nanakamado": 3,
    "kaede": 4,
    "hannnoki": 5,
    "haimatsu": 6
}

# Load the results
res2012 = np.load("results/2012_5x5.npy")
#res2015 = np.load("results/2015.npy")
res2021 = np.load("results/2021_5x5.npy")

diff_cmap = ListedColormap(
    np.array(
        [[0, 0, 0, 0], # 0: No change 
        [51, 221, 255, 255], # 1: Decrease
        [255, 51, 51, 255]] # 2: Increase
    ) / 255
)

## Calculate the difference of haimatsu
diff_haimatsu = np.zeros_like(res2012)
diff_haimatsu[(res2012 == labels["haimatsu"]) & (res2021 != labels["haimatsu"])] = 1 # decrease
diff_haimatsu[(res2012 != labels["haimatsu"]) & (res2021 == labels["haimatsu"])] = 2 # increase
diff_haimatsu = diff_haimatsu.astype(np.uint8)
np.save("results/diff_haimatsu_5x5.npy", diff_haimatsu)
plt.imsave("results/diff_haimatsu_5x5.png", diff_haimatsu, cmap=diff_cmap)

# Calculate the difference of sasa
diff_sasa = np.zeros_like(res2012)
diff_sasa[(res2012 == labels["sasa"]) & (res2021 != labels["sasa"])] = 1 # decrease
diff_sasa[(res2012 != labels["sasa"]) & (res2021 == labels["sasa"])] = 2 # increase
diff_sasa = diff_sasa.astype(np.uint8)
np.save("results/diff_sasa_5x5.npy", diff_sasa)
plt.imsave("results/diff_sasa_5x5.png", diff_sasa, cmap=diff_cmap)