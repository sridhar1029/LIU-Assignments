function [train_data, val_data] = split_data
% This function splits the data into train and validation
% reading data into dat variable
dat = csvread('data/optdigits_train.txt');

% spliting data into train and validation to choose best k for the model
% I have chose the split size of 75%. (75% for train and 25% for validation)
dims = size(dat);
n = dims(1);
split_precentage = 75;
tr_siz = ceil(n*split_precentage/100);

X_train = dat(1:tr_siz, 1:(end-1));
Y_train = dat(1:tr_siz, end);
X_val = dat((tr_siz+1):end, 1:(end-1));
Y_val = dat((tr_siz+1):end, end);
train_data = struct('X', X_train, 'Y', Y_train);
val_data = struct('X_val', X_val, 'Y_val', Y_val);
end