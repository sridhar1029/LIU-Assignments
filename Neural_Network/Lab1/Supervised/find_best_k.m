%% This script finds the best value for k using cross validation

%% Select which data to use:

% 1 = dot cloud 1
% 2 = dot cloud 2
% 3 = dot cloud 3
% 4 = OCR data

dataSetNr = 4; % Change this to load new data 

[X, D, L] = loadDataSet( dataSetNr );

%% Select a subset of the training features

numBins = 2; % Number of Bins you want to devide your data into
numSamplesPerLabelPerBin = 100; % Number of samples per label per bin, set to inf for max number (total number is numLabels*numSamplesPerBin)
selectAtRandom = true; % true = select features at random, false = select the first features

[ Xt, Dt, Lt ] = selectTrainingSamples(X, D, L, numSamplesPerLabelPerBin, numBins, selectAtRandom );

%% Finding best k
% finding the best value for k using cross validation
% we will run a loop from 1 to 50 and will select the best k value based on
% the validation accuracy

val_acc = zeros(50, 2);

for i = 1:50
    LkNN = kNN(Xt{2}, i, Xt{1}, Lt{1});
    cM = calcConfusionMatrix( LkNN, Lt{2});
    acc = calcAccuracy(cM);
    val_acc(i, 1) = i;
    val_acc(i, 2) = acc;
end

plot(val_acc(:, 1), val_acc(:, 2));
axis([0, 50, 0, 1.2]);


%% Calculate The Confusion Matrix and the Accuracy
% Note: you have to modify the calcConfusionMatrix() function yourselfs.
[~, best_k] = max(val_acc(:,2));
LkNN = kNN(Xt{2}, best_k, Xt{1}, Lt{1});
fprintf("The best value of k selected was %i", best_k);
% The confucionMatrix
cM = calcConfusionMatrix( LkNN, Lt{2})

% The accuracy
acc = calcAccuracy(cM)