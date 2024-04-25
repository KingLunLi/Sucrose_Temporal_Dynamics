"""
DeepLabCut Toolbox (deeplabcut.org)
© A. & M. Mathis Labs

Licensed under GNU Lesser General Public License v3.0
"""


from dlclive.processor.processor import Processor
import serial
import struct
import pickle
import time
import os
import sys


import datetime
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import torchvision.models as models
import torchvision.transforms as transforms
import torch.utils.data as data
import torchvision
from torch.autograd import Variable
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import OneHotEncoder, LabelEncoder
from sklearn.metrics import accuracy_score
from PIL import Image, ImageSequence
from torch.utils import data
from tqdm import tqdm
import pandas as pd
import random

result_time = []
result = []
result2 = []
result_datetime =[]
result_datetime2 =[]

print('3dcnn')
class Dataset_3DCNN(data.Dataset):
    "Characterizes a dataset for PyTorch"
    def __init__(self, data,  frames, bSampling, minD, maxD, offset):
        "Initialization"
        
        self.frames=31

        data1=data

        labels=[]
        index=0
        count=1
        page =np.ndarray(shape=(count, self.frames,20), dtype=float, order='F')
        page_d=np.ndarray(shape=(count, self.frames,20), dtype=float, order='F')
        index1=0
        index2=0
        sum1=0

        count=int(data1.shape[0]/self.frames)

        for index, row in data1.iterrows():
            index1=int(index/self.frames)
            index2=int(index % self.frames)
            if True: 

                
                page[index1, index2,0]=row["nose_x"]
                page[index1, index2,1]=row["nose_y"]
                page[index1, index2,2]=row["objectA_x"]
                page[index1, index2,3]=row["objectA_y"]
                page[index1, index2,4]=row["left ear_x"]
                page[index1, index2,5]=row["left ear_y"]
                page[index1, index2,6]=row["right ear_x"]
                page[index1, index2,7]=row["right ear_y"]
                page[index1, index2,8]=row["neck_x"]
                page[index1, index2,9]=row["neck_y"]
                page[index1, index2,10]=row["middle back_x"]
                page[index1, index2,11]=row["middle back_y"]
                page[index1, index2,12]=row["middle left_x"]
                page[index1, index2,13]=row["middle left_y"]
                page[index1, index2,14]=row["middle right_x"]
                page[index1, index2,15]=row["middle right_y"]
                page[index1, index2,16]=row["tail buttom_x"]
                page[index1, index2,17]=row["tail buttom_y"]
                page[index1, index2,18]=row["tail mid_x"]
                page[index1, index2,19]=row["tail mid_y"]
                

            
            if index %self.frames==0:
                label="1"
                if label=='R1' :
                    labels.append(0)
                else:
                    labels.append(1) 
                    sum1=sum1+1
        page_d=torch.from_numpy(page)
        page_d=page_d.to(torch.float32)
        self.data1=page_d
        self.labels = labels      
                


    def __len__(self):
        "Denotes the total number of samples"
        return len(self.labels)

    def __getitem__(self, index):
        "Generates one sample of data"
        # Select sample
        # Load data
        X = self.data1[index]#.unsqueeze_(0)     # (input) spatial images
        y = torch.LongTensor([self.labels[index]])                  # (labels) LongTensor are for int64 instead of FloatTensor

        # print(X.shape)
        return X, y
## ---------------------- end of Dataloaders ---------------------- ##
print('LSTMClassifier')

            
class LSTMClassifier(nn.Module):
    """Very simple implementation of LSTM-based time-series classifier."""
    
    def __init__(self, input_dim, hidden_dim, layer_dim, output_dim):
        super().__init__()
        self.hidden_dim = hidden_dim
        self.layer_dim = layer_dim
        self.rnn = nn.LSTM(input_dim, hidden_dim, layer_dim, batch_first=True)
        self.fc = nn.Linear(hidden_dim, output_dim)
        self.batch_size = None
        self.hidden = None
    
    def forward(self, x):
        h0, c0 = self.init_hidden(x)
        out, (hn, cn) = self.rnn(x, (h0, c0))
        out = self.fc(out[:, -1, :])
        return out
    
    def init_hidden(self, x):
        h0 = torch.zeros(self.layer_dim, x.size(0), self.hidden_dim)
        c0 = torch.zeros(self.layer_dim, x.size(0), self.hidden_dim)
        return [t.cuda() for t in (h0, c0)]
print('train')  
def train(log_interval, model, device, train_loader, optimizer, epoch):
    # set model as training mode
    model.train()
   
    losses = []
    scores = []
    N_count = 0   # counting total trained sample in one epoch
    for batch_idx, (X, y) in enumerate(train_loader):
        # distribute data to device


        X, y = X.to(device), y.to(device).view(-1, )

        N_count += X.size(0)

        optimizer.zero_grad()
        output = model(X)  # output size = (batch, number of classes)

        loss = F.cross_entropy(output, y)
        
        losses.append(loss.item())

        # to compute accuracy
        y_pred = torch.max(output, 1)[1]  # y_pred != output
        step_score = accuracy_score(y.cpu().data.squeeze().numpy(), y_pred.cpu().data.squeeze().numpy())
        scores.append(step_score)         # computed on CPU

        loss.backward()
        optimizer.step()

        # show information
        if (batch_idx + 1) % log_interval == 0:
            print('Train Epoch: {} [{}/{} ({:.0f}%)]\tLoss: {:.6f}, Accu: {:.2f}%'.format(
                epoch + 1, N_count, len(train_loader.dataset), 100. * (batch_idx + 1) / len(train_loader), loss.item(), 100 * step_score))

    return losses, scores

print('validation')
def validation(model, device, test_loader):
    # set model as testing mode
    model.eval()

    test_loss = 0
    all_y = []
    all_y_pred = []
    with torch.no_grad():
        for X, y in test_loader:
            # distribute data to device
            X, y = X.to(device), y.to(device).view(-1, )

            output = model(X)
              # sum up batch loss
            y_pred = output.max(1, keepdim=True)[1]  # (y_pred != output) get the index of the max log-probability

            # collect all y and y_pred in all batches
            all_y.extend(y)
            all_y_pred.extend(y_pred)


    # to compute accuracy
    all_y = torch.stack(all_y, dim=0)
    all_y_pred = torch.stack(all_y_pred, dim=0)
    ypredict=all_y_pred.cpu().data.squeeze().numpy()
    # show information
    #print('\nTest set ({:d} samples): Average loss: {:.4f}, Accuracy: {:.2f}%\n'.format(len(all_y), test_loss, 100* test_score))

    # save Pytorch models of best record
    #torch.save(model.state_dict(), os.path.join(save_model_path, '3dcnn_epoch{}.pth'.format(epoch + 1)))  # save spatial_encoder
    #torch.save(optimizer.state_dict(), os.path.join(save_model_path, '3dcnn_optimizer_epoch{}.pth'.format(epoch + 1)))      # save optimizer
    #print("Epoch {} model saved!".format(epoch + 1))

    return ypredict



        



  





class TeensyLaser(Processor):
    def __init__(
        self, com, baudrate=115200, pulse_freq=50, pulse_width=5, max_stim_dur=1
    ):

        super().__init__()
        self.ser = serial.Serial('COM3',9600)
        self.pulse_freq = pulse_freq
        self.pulse_width = pulse_width
        self.max_stim_dur = (
            max_stim_dur if (max_stim_dur >= 0) and (max_stim_dur < 65356) else 0
        )
        
        self.saved_data = np.zeros([31,10,3])
        self.saved_data2 = np.zeros([400000,10,3])
        self.currentindex= 0
        self.random = False
        self.writetime = 0
        self.stim_on = False
        self.stim_on_time = []
        self.stim_off_time = []
        self.ser.write(b"X")
        batch_size=200
        # Detect devices
        print('detect device')
        use_cuda = torch.cuda.is_available()                   # check if GPU exists
        # use CPU or GPU
        self.params = {'batch_size': batch_size, 'shuffle': True, 'num_workers': 0, 'pin_memory': True} if use_cuda else {}
# load UCF101 actions names
        





        self.randomnum = 0
        self.resetnum = 1
        input_dim = 20  
        hidden_dim = 256
        layer_dim = 8
        output_dim = 2
        seq_dim = 60




        best_acc = 0
        patience, trials = 100, 0

        self.model = LSTMClassifier(input_dim, hidden_dim, layer_dim, output_dim)
        self.model = self.model.cuda()

# input saved model
        self.model.load_state_dict(torch.load("C:/Xiguang/05smodel_D2_updated.pth"))
#optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)   # optimize all cnn parameters# check if GPU exists
        self.device = torch.device("cuda" if use_cuda else "cpu")   # use CPU or GPU
        
   
    def close_serial(self):

        self.ser.close()

    def turn_stim_on(self):

        # command to activate PWM signal to laser is the letter 'O' followed by three 16 bit integers -- pulse frequency, pulse width, and max stim duration
        if not self.stim_on:
            self.ser.write(b"O")
            self.stim_on = True
            self.stim_on_time.append(time.time())

    def turn_stim_off(self):

        # command to turn off PWM signal to laser is the letter 'X'
        if self.stim_on:
            self.ser.write(b"X")
            self.stim_on = False
            self.stim_off_time.append(time.time())

    def remove_likelyhood(self):
        self.valid_data=self.saved_data[:,:,[0,1]]
    def process(self, pose, **kwargs):

        # define criteria to stimulate (e.g. if first point is in a corner of the video)
        if self.currentindex<31:
            self.saved_data[self.currentindex,:,:]=pose
            self.currentindex=self.currentindex+1
            self.saved_data2[self.currentindex,:,:]=pose
        else:
            self.saved_data2[self.currentindex,:,:]=pose
            self.saved_data[range(30),:,:]=self.saved_data[range(1,31),:,:]
            self.saved_data[30,:,:]=pose
            self.currentindex=self.currentindex+1
        now = datetime.datetime.now() 
        result_datetime2.append(str(now))
        if self.currentindex>30:
            self.remove_likelyhood()
            valid_data = self.valid_data
            valid_data = valid_data.reshape(31,20)
            valid_data = pd.DataFrame(valid_data)
            valid_data.columns = ["nose_x","nose_y","objectA_x","objectA_y","left ear_x","left ear_y","right ear_x","right ear_y","neck_x","neck_y","middle back_x","middle back_y","middle left_x","middle left_y","middle right_x","middle right_y","tail buttom_x","tail buttom_y","tail mid_x","tail mid_y"]
            valid_set = Dataset_3DCNN(valid_data,31, bSampling=False,minD=30, maxD=80,offset=0 )
            valid_loader = data.DataLoader(valid_set, **self.params) 
            ypredict1 = validation(self.model, self.device, valid_loader)
            r2value=0
            if self.resetnum>30 and self.resetnum<=60:
                resetvalue = self.resetnum-30
            else:
                resetvalue = 30
            if ypredict1==1:
                result.append(1)
                self.turn_stim_off()
                if self.currentindex>150 and self.random == False:        
                    if sum(result[(len(result)-resetvalue):(len(result))])>1 and self.resetnum>31:
                        self.turn_stim_on()
                        print(self.currentindex)
                        print("1")
                        r2value=1
                        self.resetnum = 0
            else:
                self.turn_stim_off()
                result.append(0)
            if self.random == True:
                self.turn_stim_off()
                if random.randint(0, 75)==0 and self.resetnum>31:
                    self.turn_stim_on()
                    print(self.currentindex)
                    print("Random")
                    self.resetnum = 0
                    self.randomnum = self.randomnum + 1
            result2.append(r2value)
            result_time.append(self.currentindex)               
            self.resetnum = self.resetnum+1
            result_datetime.append(str(now))
            self.writetime = self.writetime +1
            if self.writetime >=45000:
                date = "0319"
                df_data = pd.DataFrame(list(zip(result, result_time, result_datetime,result2)), columns = ['result', 'result_time', 'result_datetime','result2'])
                df_data.to_csv(date+str(self.currentindex)+'prediction_realscope'+'.csv', index=False)
                saved_data2 = self.saved_data2.copy()
                saved_data2 = saved_data2.reshape(400000,30)
                df_data2 = pd.DataFrame(saved_data2)
                df_data2["Time(s)"] = 0
                df_data2["Time(s)"].iloc[0:len(result_datetime2)] = result_datetime2
                df_data2.to_csv(date+'cordinates'+str(self.currentindex)+'realscope'+'.csv', index=False)
                if self.random == True:
                    df_data3 = pd.DataFrame([self.randomnum], columns = ['result'])
                    df_data3.to_csv(date+'random'+str(self.currentindex)+'.csv', index=False)
                self.writetime = 0

        return pose

    def save(self, file=None):

        ### save stim on and stim off times
        save_code = 0
        if file:
            try:
                pickle.dump(
                    {"stim_on": self.stim_on_time, "stim_off": self.stim_off_time},
                    open(file, "wb"),
                )
                save_code = 1
            except Exception:
                save_code = -1
        return save_code
