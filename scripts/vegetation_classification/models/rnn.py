from torch import nn
from torch.nn import functional as F
from torch import optim
from torchvision import transforms
from models.nnmodel import NNClasifier

class RNN(nn.Module):
    def __init__(self, x_dim, y_dim):
        super().__init__()
        self.h_dim = int((x_dim + y_dim) / 2)
        self.fc0 = nn.Linear(x_dim, self.h_dim)
        self.bn0 = nn.BatchNorm1d(self.h_dim)
        self.gelu0 = nn.GELU()
        self.lstm = nn.LSTM(self.h_dim, self.h_dim, batch_first=True, bidirectional = False)
        self.bn1 = nn.BatchNorm1d(self.h_dim)
        self.do1 = nn.Dropout(0.0)
        self.gelu1 = nn.GELU()
        self.fc1 = nn.Linear(self.h_dim, self.h_dim)
        self.bn2 = nn.BatchNorm1d(self.h_dim)
        self.do2 = nn.Dropout(0.0)
        self.gelu2 = nn.GELU()
        self.fc2 = nn.Linear(self.h_dim, y_dim)

    def forward(self, x):
        bs, l, _ = x.shape
        h = self.fc0(x)
        h = h.reshape(-1, self.h_dim)
        h = self.bn0(h)
        h = h.reshape(bs, l, self.h_dim)
        h = self.gelu0(h)
        _, h = self.lstm(h)
        h = h[0].view(-1, self.h_dim)
        h = self.bn1(h)
        h = self.do1(h)
        h = self.gelu1(h)
        h = self.fc1(h)
        h = self.bn2(h)
        h = self.do2(h)
        self.gelu2(h)
        return F.softmax(self.fc2(h), dim=1)

class RNNClassifier(NNClasifier):
    def __init__(self, data_dir, labels_dir, batch_size, device="cuda", num_workers=20, label="all", test_size=0.2, cmap="jet"):
        super().__init__(data_dir, labels_dir, batch_size, device, num_workers, label, test_size, cmap)
        self.tf_train = transforms.Compose([])
        self.tf_valid = transforms.Compose([])
        x, y = next(iter(self.train_loader))
        self.x_shape = x.shape[2]
        self.model = RNN(self.x_shape, self.y_dim).to(self.device)
        self.optimizer = optim.RAdam(self.model.parameters(), lr=1e-3)
        self.loss_cls = nn.CrossEntropyLoss()
    
    def get_instance(self):
        return RNN(self.x_shape, self.y_dim).to(self.device)