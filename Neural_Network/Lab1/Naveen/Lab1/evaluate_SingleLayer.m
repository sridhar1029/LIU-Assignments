%% This script will help you test out your single layer neural network code

%% Select which data to use:

%% Select which data to use:

% 1 = dot cloud 1
% 2 = dot cloud 2
% 3 = dot cloud 3
% 4 = OCR data

dataSetNr = 1; % Change this to load new data 

[X, D, L] = loadDataSet( dataSetNr );

%% Select a subset of the training features

numBins = 2; % Number of Bins you want to divide your data into
numSamplesPerLabelPerBin = 100; % Number of samples per label per bin, set to inf for max number (total number is numLabels*numSamplesPerBin)
selectAtRandom = true; % true = select features at random, false = select the first features

[ Xt, Dt, Lt ] = selectTrainingSamples(X, D, L, numSamplesPerLabelPerBin, numBins, selectAtRandom );

% Note: Xt, Dt, Lt will be cell arrays, to extract a bin from them use i.e.
% XBin1 = Xt{1};
%% 

% The Training Data
dim_train =  size(Xt{1},2);            % Get as many as vector biases as there are neuron in current layer
Xtraining = [ones(1,dim_train);Xt{1}]; % Bias is appended horizontally to the matrix. 
                    

% The Test Data
dim_test =  size(Xt{2},2);             
Xtest = [ones(1,dim_test);Xt{2}];      %Bias is appended horizontally to the matrix


%% Train your single layer network

numIterations = 1000; % Number of iterations (Epochs)
learningRate = 0.0001; % Learning rate

a=-1;b=1;
W0 =  a + (b-a)*rand(size(Dt{1},1),size(Xtraining,1),1);   % Weight matrix of 2*3. 2 output and 3 inputs 
W0 = W0/10;
 
[W, trainingError, testError ] = trainSingleLayer(Xtraining,Dt{1},Xtest,Dt{2},W0,numIterations, learningRate );
%%
% Plot errors
figure(1101)
clf
[mErr, mErrInd] = min(testError);
plot(trainingError,'k','linewidth',1.5)
hold on
plot(testError,'r','linewidth',1.5)
plot(mErrInd,mErr,'bo','linewidth',1.5)
hold off
title('Training and Test Errors, Single Layer')
legend('Training Error','Test Error','Min Test Error')

%% Calculate The Confusion Matrix and the Accuracy of the Evaluation Data
% Note: you have to modify the calcConfusionMatrix() function yourselfs.

[ Y, LSingleLayerTraining ] = runSingleLayer(Xtraining, W);
[ Y, LSingleLayerTest ] = runSingleLayer(Xtest, W);

% The confusion Matrix
cM = calcConfusionMatrix( LSingleLayerTest, Lt{2});

% The accuracy
acc = calcAccuracy(cM)

%% Plot classifications
% Note: You do not need to change this code.

if dataSetNr < 4
    plotResultSingleLayer(W,Xtraining,Lt{1},LSingleLayerTraining,Xtest,Lt{2},LSingleLayerTest)
else
    plotResultsOCR( Xtest, Lt{2}, LSingleLayerTest)
end
