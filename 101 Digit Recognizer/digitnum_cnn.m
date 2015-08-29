%function test_example_CNN
%load mnist_uint8;

%test = csvread('test.csv', 1, 0);

clear ; close all; clc

load('digitdata.mat')

casenum = 42000
tmp = randperm(size(train, 1), casenum);
train_x = train(tmp, 2:end);
label = train(tmp, 1);
test_x = test;

m = size(label, 1)
train_y = zeros(m, 10);
for i=1:m
	train_y(i, label(i)+1) = 1;
end
train_x = double(reshape(train_x',28,28,casenum))/255;
test_x = double(reshape(test_x',28,28,28000))/255;
train_y = double(train_y');
%test_y = double(test_y');

%% ex1 Train a 6c-2s-12c-2s Convolutional neural network 
%will run 1 epoch in about 200 second and get around 11% error. 
%With 100 epochs you'll get around 1.2% error

rand('state',0)

cnn.layers = {
    struct('type', 'i') %input layer
    struct('type', 'c', 'outputmaps', 6, 'kernelsize', 5) %convolution layer
    struct('type', 's', 'scale', 2) %sub sampling layer
    struct('type', 'c', 'outputmaps', 12, 'kernelsize', 5) %convolution layer
    struct('type', 's', 'scale', 2) %subsampling layer
};


opts.alpha = 1;
opts.batchsize = 50;
opts.numepochs = 200;

cnn = cnnsetup(cnn, train_x, train_y);
cnn = cnntrain(cnn, train_x, train_y, opts);

clear train train_x train_y
test_y = cnnff(cnn, test_x);
[~, y] = max(test_y.o);
y = y - 1;
y = y'
csvwrite('pre.csv', y);

