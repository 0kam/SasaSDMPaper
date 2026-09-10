from models.crnn import CRNNClassifier
import tensorboardX
from matplotlib.colors import ListedColormap
import numpy as np
from matplotlib import pyplot as plt
from glob import glob
import os
import torch

cmap = ListedColormap(
    np.array(
        [[154,205,50], # 1: Dwarf bamboo
        [70, 130, 180], # 2: Other vegetation
        [192,192,192], # 3: No vegetation
        [220,20,60], # 4: ナナカマド
        [255,215,0], # 5: ミネカエデ
        [139,69,19],
        [0,100,0]] # 4: Dwarf pine
    ) / 255
)

years = ["2012", "2021"]
kernel_size = (5,5)

for year in years:
    rnn = CRNNClassifier(f"../data/{year}_{kernel_size[0]}x{kernel_size[1]}", "../data_source/labels", 500, test_size=0.2, cmap=cmap, kernel_size=kernel_size)
    rnn.draw_teacher("../results/teacher.png", (5616, 3744), shrink=3)
    rnn.kfold(50, f"cv/{year}_{kernel_size[0]}x{kernel_size[1]}/", k=5, shuffle=True)
 
    rnn = CRNNClassifier(f"../data/{year}_{kernel_size[0]}x{kernel_size[1]}", "../data_source/labels", 500, test_size=0.2, cmap=cmap, kernel_size=kernel_size)

    for d in glob(f"runs/cv/{year}_{kernel_size[0]}x{kernel_size[1]}/fold_*"):
        print(d)
        rnn.load(d + "/best.pth")
        res = rnn.draw(f"../data_source/aligned/{year}", d + "/pred.png", kernel_size, 5000)
        np.save(d + "/pred.npy", res[0])
        plt.imsave(d + "/pred.png", res[0], cmap = cmap)

    preds = []
    for d in glob(f"runs/cv/{year}_{kernel_size[0]}x{kernel_size[1]}/*"):
        if os.path.isdir(d):
            preds.append(np.load(d + "/pred.npy"))

    preds = torch.Tensor(np.stack(preds))
    pred, _ = torch.mode(preds, dim = 0)
    np.save(f"../results/{year}_{kernel_size[0]}x{kernel_size[1]}.npy", pred)
    plt.imsave(f"../results/{year}_{kernel_size[0]}x{kernel_size[1]}.png", pred, cmap=cmap)