from torch import nn
from torch.nn import functional as F
from torch import optim
from torchvision import transforms
from models.nnmodel import NNClasifier

class CRNN(nn.Module):
    def __init__(self, x_shape, y_dim):
        super().__init__()
        self.x_shape = x_shape # [seq_len, channels, height, width]
        self.conv1 = nn.Conv2d(3, 8, (3,3), 1)
        self.bn2d_1 = nn.BatchNorm2d(8)
        self.prelu1 = nn.PReLU()
        self.maxpool1 = nn.MaxPool2d((3,3), 1)       
        conv_out_shape = (x_shape[2]-4) * (x_shape[3]-4) * 8
        self.h_dim = int((conv_out_shape + y_dim) / 2)
        self.lstm = nn.LSTM(conv_out_shape, self.h_dim, batch_first=True)
        self.bn1 = nn.BatchNorm1d(self.h_dim)
        self.do1 = nn.Dropout(0.0)
        self.fc1 = nn.Linear(self.h_dim, self.h_dim)
        self.prelu2 = nn.PReLU()
        self.bn2 = nn.BatchNorm1d(self.h_dim)
        self.do2 = nn.Dropout(0.0)
        self.fc2 = nn.Linear(self.h_dim, y_dim)    

    def forward(self, x):
        x = x.reshape(-1, self.x_shape[1], self.x_shape[2], self.x_shape[3])
        h = self.conv1(x)
        h = self.prelu1(h)
        h = self.bn2d_1(h)
        h = self.maxpool1(h)
        h = h.view((-1, self.x_shape[0], h.shape[1]*h.shape[2]*h.shape[3]))
        _, h = self.lstm(h)
        h = h[0].view(-1, self.h_dim)
        h = self.bn1(h)
        h = self.do1(h)
        h = self.prelu2(self.fc1(h))
        h = self.bn2(h)
        h = self.do2(h)
        return F.softmax(self.fc2(h), dim=1)

class CRNNClassifier(NNClasifier):
    def __init__(self, data_dir, labels_dir, batch_size, device="cuda", num_workers=20, label="all", test_size=0.2, cmap="jet", kernel_size=(5,5)):
        super().__init__(data_dir, labels_dir, batch_size, device, num_workers, label, test_size, cmap, kernel_size)
        self.tf_train = transforms.Compose([])
        self.tf_valid = transforms.Compose([])
        x, y = next(iter(self.train_loader))
        seq_len = int(x.shape[1] / 3 / (kernel_size[0] * kernel_size[1]))
        self.x_shape = (seq_len, 3) + kernel_size # [seq_len, channels, height, width]
        self.model = CRNN(self.x_shape, self.y_dim).to(self.device)
        self.optimizer = optim.RAdam(self.model.parameters(), lr=1e-3)
        self.loss_cls = nn.CrossEntropyLoss()
    
    def get_instance(self):
        return CRNN(self.x_shape, self.y_dim).to(self.device)