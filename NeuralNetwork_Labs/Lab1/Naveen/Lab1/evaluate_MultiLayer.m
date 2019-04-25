%% This script will help you test out your single layer neural network code

%% Select which data to use:

% 1 = dot cloud 1
% 2 = dot cloud 2
% 3 = dot cloud 3
% 4 = OCR data

dataSetNr = 3; % Change this to load new data 

[X, D, L] = loadDataSet( dataSetNr );

%% Select a subset of the training features

numBins = 2; % Number of Bins you want to devide your data into
numSamplesPerLabelPerBin = inf; % Number of samples per label per bin, set to inf for max number (total number is numLabels*numSamplesPerBin)
selectAtRandom = true; % true = select features at random, false = select the first features

[ Xt, Dt, Lt ] = selectTrainingSamples(X, D, L, numSamplesPerLabelPerBin, numBins, selectAtRandom );

% Note: Xt, Dt, Lt will be cell arrays, to extract a bin from them use i.e.
% XBin1 = Xt{1};

 Xt{1} = Xt{1}(:,300:600);
 Dt{1} = Dt{1}(:,300:600);
 Lt{1} = Lt{1}(300:600,:);
%% Modify the X Matrices so that a bias is added

% The Training Data
dim_train =  size(Xt{1},2);       % Get as many as vector biases as there are neuron in current layer
Xtraining = [ones(1,dim_train);Xt{1}];

% The Test Data
dim_test =  size(Xt{2},2);             
Xtest = [ones(1,dim_test);Xt{2}];      %Bias is appended horizontally to the matrix



%% Train your single layer network
% Note: You nned to modify trainSingleLayer() in order to train the network

numHidden = 20; % Change this, Number of hidde neurons 
numIterations = 600; % Change this, Numner of iterations (Epochs)
learningRate = 0.0004; % Learningrate

a=-1;b=1

rng(3)
W0 = (b-a)*rand(numHidden,size(Xtraining,1),1)+a; % Weight of inner hidden layer n*3
V0 = (b-a)*rand(size(Dt{1},1),numHidden+1,1)+a;  % Weight of outer layer  2*(n+1)

%W0 = normrnd(0,1,size(W0));
%V0 = normrnd(0,1,size(V0));

%fprintf("%i",W0)
%fprintf("%i",V0)

W0=W0/10;
V0=V0/10;

%
tic;
[W,V, trainingError, testError ] = trainMultiLayer(Xtraining,Dt{1},Xtest,Dt{2}, W0,V0,numIterations, learningRate );
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
tic
[ Y, LMultiLayerTest ] = runMultiLayer(Xtest, W,V);
classificationTime = toc/length(Xtest);
% The confucionMatrix
cM = calcConfusionMatrix( LMultiLayerTest, Lt{2});

% The accuracy
acc = calcAccuracy(cM);

display(['Time spent training: ' num2str(trainingTime) ' sec'])
display(['Time spent classifying 1 feature vector: ' num2str(classificationTime) ' sec'])
display(['Accuracy: ' num2str(acc)])

%% Plot classifications
% Note: You do not need to change this code.

if dataSetNr < 4
    plotResultMultiLayer(W,V,Xtraining,Lt{1},LMultiLayerTraining,Xtest,Lt{2},LMultiLayerTest)
else
    plotResultsOCR( Xtest, Lt{2}, LMultiLayerTest )
end
