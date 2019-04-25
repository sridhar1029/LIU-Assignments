%% This script will help you test out your single layer neural network code

%% Select which data to use:
clc
% 1 = dot cloud 1
% 2 = dot cloud 2
% 3 = dot cloud 3
% 4 = OCR data
rng(123456)
dataSetNr = 4; % Change this to load new data 

[X, D, L] = loadDataSet( dataSetNr );

%% Select a subset of the training features

numBins = 2; % Number of Bins you want to devide your data into
numSamplesPerLabelPerBin = inf; % Number of samples per label per bin, set to inf for max number (total number is numLabels*numSamplesPerBin)
selectAtRandom = true; % true = select features at random, false = select the first features

[ Xt, Dt, Lt ] = selectTrainingSamples(X, D, L, numSamplesPerLabelPerBin, numBins, selectAtRandom );

% Note: Xt, Dt, Lt will be cell arrays, to extract a bin from them use i.e.
% XBin1 = Xt{1};
%% Modify the X Matrices so that a bias is added
if dataSetNr == 4
    Xt{1} = Xt{1}/max(max(Xt{1}));
    Xt{2} = Xt{2}/max(max(Xt{1}));
end
% The Training Data
dim_tr = size(Xt{1}, 2);
Xtraining = [ones(1, dim_tr); Xt{1}];

% The Test Data
dim_te = size(Xt{2}, 2);
Xtest = [ones(1, dim_te); Xt{2}];


%% Train your single layer network
% Note: You nned to modify trainMultiLayer() in order to train the network
overfit=false;
if(dataSetNr==1)
    numHidden = 20; % Change this, Number of hidden neurons 
    numIterations = 1000; % Change this, Numner of iterations (Epochs)
    learningRate = 0.009; % Change this, Your learningrate
elseif(dataSetNr==2)
    numHidden = 30; % Change this, Number of hidden neurons 
    numIterations = 1000; % Change this, Numner of iterations (Epochs)
    learningRate = 0.009; % Change this, Your learningrate
elseif(dataSetNr==3)
    numHidden = 20; % Change this, Number of hidden neurons 
    numIterations = 1000; % Change this, Numner of iterations (Epochs)
    learningRate = 0.09; % Change this, Your learningrate
elseif(dataSetNr==4 && overfit==true)
    numHidden = 50; % Change this, Number of hidden neurons 
    numIterations = 1000; % Change this, Numner of iterations (Epochs)
    learningRate = 0.01; % Change this, Your learningrate
elseif(dataSetNr==4 && overfit==false)
    numHidden = 40; % Change this, Number of hidden neurons 
    numIterations = 1000; % Change this, Numner of iterations (Epochs)
    learningRate = 0.001; % Change this, Your learningrate
end

W0 = randn(numHidden, size(Xtraining, 1))/10000; % Change this, Initiate your weight matrix W
W0(:, 1) = 0;
V0 = randn(size(Dt{1}, 1), numHidden+1)/10000; % Change this, Initiate your weight matrix V
V0(:, 1) = 0;

%
tic
[W,V, trainingError, testError ] = trainMultiLayer(Xtraining,Dt{1},Xtest,Dt{2}, Lt, W0,V0,numIterations, learningRate );
trainingTime = toc;
%% Plot errors
figure(1101)
clf
[mErr, mErrInd] = min(testError);
plot(trainingError,'k','linewidth',1.5)
hold on
plot(testError,'r','linewidth',1.5)
plot(mErrInd,mErr,'bo','linewidth',1.5)
hold off
title('Training and Test Errors, Multi-Layer')
legend('Training Error','Test Error','Min Test Error')

%% Calculate The Confusion Matrix and the Accuracy of the Evaluation Data
% Note: you have to modify the calcConfusionMatrix() function yourselfs.

[ Y, LMultiLayerTraining ] = runMultiLayer(Xtraining, W, V);
cMTr = calcConfusionMatrix( LMultiLayerTraining, Lt{1});
accTr = calcAccuracy(cMTr);
display(['Training Accuracy: ' num2str(accTr)])
tic
[ Y, LMultiLayerTest ] = runMultiLayer(Xtest, W,V);
classificationTime = toc/length(Xtest);
% The confucionMatrix
cMTe = calcConfusionMatrix( LMultiLayerTest, Lt{2})

% The accuracy
accTe = calcAccuracy(cMTe);

display(['Time spent training: ' num2str(trainingTime) ' sec'])
display(['Time spent calssifying 1 feature vector: ' num2str(classificationTime) ' sec'])
display(['Accuracy: ' num2str(accTe)])

%% Plot classifications
% Note: You do not need to change this code.

if dataSetNr < 4
    plotResultMultiLayer(W,V,Xtraining,Lt{1},LMultiLayerTraining,Xtest,Lt{2},LMultiLayerTest)
else
    plotResultsOCR( Xtest, Lt{2}, LMultiLayerTest )
end
