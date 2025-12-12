import torch
import torch_optimizer as optim 
from sklearn.model_selection import train_test_split
from utils.utils import TrainDS, read_sses, DrawDS, draw_legend, draw_teacher
from torch.utils.data import DataLoader, TensorDataset
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import classification_report
from tqdm import tqdm
from pathlib import Path
from glob import glob
from tensorboardX import SummaryWriter
from matplotlib import pyplot as plt
from PIL import Image
import pandas as pd

class NNClasifier():
    def __init__(self, data_dir, labels_dir, batch_size, device="cuda", num_workers=20, label="all", test_size = 0.2, cmap = "jet", kernel_size = (1,1)):
        self.classes = [Path(n).name for n in glob(data_dir + "/*")]
        self.device = device
        self.batch_size = batch_size
        self.num_workers = num_workers
        _, labels = read_sses(labels_dir, (9999,9999), label=label)
        self.label = label
        self.labels = labels
        self.labels_dir = labels_dir
        self.cmap = cmap
        self.best_test_loss = 9999
        ds =  TrainDS(data_dir)
        x = torch.cat([torch.Tensor(ds[i][0]) for i in range(len(ds))])
        if kernel_size == (1,1):
            x = x.reshape(x.shape[0], -1, 3)
        y = torch.cat([torch.Tensor(ds[i][1]) for i in range(len(ds))]).to(torch.long)
        self.ds = TensorDataset(x, y)

        self.idx = list(range(len(self.ds.tensors[1])))
        self.test_size = test_size
        if self.test_size > 0:
            train_indices, val_indices = train_test_split(self.idx, test_size=self.test_size, stratify=self.ds.tensors[1], shuffle=True)
            train_dataset = torch.utils.data.Subset(self.ds, train_indices)
            val_dataset = torch.utils.data.Subset(self.ds, val_indices)
        else:
            train_dataset = self.ds
        self.y_dim = len(self.classes)
        self.train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True, num_workers=num_workers)
        if self.test_size > 0:
            self.val_loader = DataLoader(val_dataset, batch_size=batch_size, shuffle=False, num_workers=num_workers)
        keys = [str(i+1) for i in range(self.y_dim)]
        vals = [i for i in range(self.y_dim)]
        self.class_to_idx = dict(zip(keys, vals))
    
    def _train(self, epoch):
        self.model.train()
        train_loss = 0
        for x, y in tqdm(self.train_loader):
            x = self.tf_train(x)
            x = x.to(self.device)
            y = torch.eye(self.y_dim)[y].to(self.device)
            self.model.zero_grad()
            y2 = self.model(x)
            loss = self.loss_cls(y2, y.argmax(1))
            loss.backward()
            self.optimizer.step()

            train_loss += loss.item()
        train_loss = train_loss * self.train_loader.batch_size / len(self.train_loader.dataset)
        print('Epoch: {} Train loss: {:.4f}'.format(epoch, train_loss))
        return train_loss
    
    def _val(self, epoch):
        self.model.eval()
        test_loss = 0
        ys = []
        pred_ys = []
        for x, _y in self.val_loader:
            x = self.tf_valid(x)
            x = x.to(self.device)
            y = torch.eye(self.y_dim)[_y].to(self.device)
            with torch.no_grad():
                y2 = self.model(x)
            loss = self.loss_cls(y2, y.argmax(1))
            test_loss += loss
            pred_ys.append(y2.argmax(1))
            ys.append(_y)

        ys = torch.cat(ys).detach().cpu()
        pred_ys = torch.cat(pred_ys).detach().cpu()

        test_loss = test_loss * self.val_loader.batch_size / len(self.val_loader.dataset)
        r = classification_report(ys, pred_ys, output_dict=True, zero_division = 0)
        res = {}
        for c, i in self.class_to_idx.items():
            c = self.labels.query("classIndex == {}".format(c))["label"].item()
            res[c] = r[str(i)]
        res["macro avg"] = r["macro avg"]
        res["weighted avg"] = r["weighted avg"]
        print("Test Loss:", str(test_loss), "F1 macro:", res["macro avg"]["f1-score"])
        return test_loss, res
    
    def train(self, epochs, log_dir):
        writer = SummaryWriter("./runs/" + log_dir)
        self.best_test_loss = 9999
        for epoch in range(1, epochs + 1):
            train_loss = self._train(epoch)
            if self.test_size > 0:
                val_loss, res = self._val(epoch)
                if val_loss < self.best_test_loss:
                    self.best_model = self.get_instance()
                    self.best_model.load_state_dict(self.model.state_dict())
                    self.best_metrics = res
                    self.best_test_loss = val_loss
                    torch.save(self.best_model.state_dict(), "./runs/"+log_dir+"/best.pth")
                writer.add_scalars("loss", {"test_loss": val_loss, "train_loss": train_loss}, epoch)
                for c, r in res.items():
                    r = {k: v for k, v in r.items() if k.lower() != "support"}
                    writer.add_scalars("val_" + c, r, epoch)
        if self.test_size > 0:
            writer.export_scalars_to_json("./runs/" + log_dir + "/all_scalars.json")
        else:
            torch.save(self.model.state_dict(), "./runs/"+log_dir+"/best.pth")
    
    def kfold(self, epochs, log_dir, k=5, shuffle = True):
        kf = StratifiedKFold(n_splits=k, shuffle=shuffle)
        results = []
        for fold, (train_indices, val_indices) in enumerate(kf.split(self.idx, self.ds.tensors[1])):
            # Reset the dataloaders and the model
            train_dataset = torch.utils.data.Subset(self.ds, train_indices)
            val_dataset = torch.utils.data.Subset(self.ds, val_indices)
            self.train_loader = DataLoader(train_dataset, batch_size=self.batch_size, shuffle=True, num_workers=self.num_workers)
            self.val_loader = DataLoader(val_dataset, batch_size=self.batch_size, shuffle=False, num_workers=self.num_workers)
            self.model = self.get_instance()
            self.optimizer = optim.RAdam(self.model.parameters(), lr=1e-3)
            # train
            self.train(epochs, log_dir+"/fold_"+str(fold))
            df = pd.DataFrame(self.best_metrics)
            df["metrics"] = df.index
            df = pd.melt(df, id_vars="metrics", var_name="vegetation", value_name="value")
            df["fold"] = fold
            results.append(df)
        pd.concat(results).to_csv("./runs/" + log_dir + "/stratified_cv.csv")
    
    def draw(self, image_dir, out_path, kernel_size, batch_size):
        with Image.open(glob(image_dir+"/*")[0]) as img:
            w, h = img.size
        dataset = DrawDS(image_dir, kernel_size)
        loader = DataLoader(dataset, batch_size, shuffle=False, num_workers=0)
        pred_ys = []
        confs = []
        self.best_model.eval()
        with torch.no_grad():
            for x in tqdm(loader):
                x = self.tf_valid(x)
                x = x.to(self.device)
                y = self.best_model(x)
                pred_y = y.argmax(1).detach().cpu()
                conf = y.max(1)[0].detach().cpu()
                pred_ys.append(pred_y)
                confs.append(conf)
        
        seg_image = torch.cat(pred_ys).reshape([h,w]).numpy()
        confs = torch.cat(confs).reshape([h,w]).numpy()
        plt.imsave(out_path, seg_image, cmap = self.cmap)
        return seg_image, confs
    
    def draw_teacher(self, out_path, image_size, shrink=0, hmat=None, mask=None):
        draw_teacher(out_path, self.labels_dir, self.class_to_idx, image_size, \
            self.label, cmap = self.cmap)
    
    def draw_legend(self, out_path):
        draw_legend(out_path, self.labels_dir, self.class_to_idx, self.label, cmap = self.cmap)
    
    def load(self, path):
        self.best_model = self.get_instance()
        self.best_model.load_state_dict(torch.load(path))
    
    def get_instance(self):
        pass