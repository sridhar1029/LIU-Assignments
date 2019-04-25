function [Wout,Vout, trainingError, testError ] = trainMultiLayer(Xtraining,Dtraining,Xtest,Dtest, Lt, W0, V0,numIterations, learningRate )
%TRAINMULTILAYER Trains the network (Learning)
%   Inputs:
%               X* - Trainin/test features (matrix)
%               D* - Training/test desired output of net (matrix)
%               V0 - Weights of the output neurons (matrix)
%               W0 - Weights of the output neurons (matrix)
%               numIterations - Number of learning setps (scalar)
%               learningRate - The learningrate (scalar)
%
%   Output:
%               Wout - Weights after training (matrix)
%               Vout - Weights after training (matrix)
%               trainingError - The training error for each iteration
%                               (vector)
%               testError - The test error for each iteration
%                               (vector)

% Initiate variables
trainingError = nan(numIterations+1,1);
testError = nan(numIterations+1,1);
trainingAcc = nan(numIterations+1,1);
testAcc = nan(numIterations+1,1);
numTraining = size(Xtraining,2);
numTest = size(Xtest,2);
numClasses = size(Dtraining,1) - 1;
Wout = W0;
Vout = V0;
lambda = 0.009;
regW = sum(Wout(:).^2);
regV = sum(Vout(:).^2);
regCof = lambda/numTraining;

% Calculate initial error
Ytraining = runMultiLayer(Xtraining, W0, V0);
Ytest = runMultiLayer(Xtest, W0, V0);
trainingError(1) = sum(sum((Ytraining - Dtraining).^2))/(numTraining*numClasses) + regCof*(regW + regV);
testError(1) = sum(sum((Ytest - Dtest).^2))/(numTest*numClasses) + regCof*(regW + regV);

for n = 1:numIterations
    if mod(n, 1000)==0 && n~=0
        learningRate = learningRate/2;
        n
    end
    prev = 1;
    batchSize = 10;
    for i = batchSize:batchSize:numTraining 
        Ytraining = runMultiLayer(Xtraining(:, prev:i), Wout, Vout);

        %forward prop start
        Z1 = Wout*Xtraining(:, prev:i);
        A1 = tanh(Z1);
        dim_a1 = size(A1, 2);
        A1_b = [ones(1, dim_a1); A1];
        %Z2 = Vout*A1_b;
        %end

        %backprop start
        con = (2/(numClasses));
        dZ2 = Ytraining - Dtraining(:, prev:i);
        dW2 = con*dZ2*(A1_b');
        dA1 = Vout(:,2:end);
        dZ1 = 1 - (A1.^2);
        dW1 = con*((dA1'*dZ2).*(dZ1))*Xtraining(:, prev:i)';
        %end

        grad_v = dW2;
        grad_w = dW1;

        Wout = Wout - learningRate * grad_w + regCof.*Wout; %Take the learning step.
        Vout = Vout - learningRate * grad_v + regCof.*Vout; %Take the learning step.
        prev = i;
    end

    [Ytraining, lblTrain] = runMultiLayer(Xtraining, Wout, Vout);
    [Ytest, lblTest] = runMultiLayer(Xtest, Wout, Vout);

    trainingError(1+n) = sum(sum((Ytraining - Dtraining).^2))/(numTraining*numClasses) + regCof*(regW + regV);
    testError(1+n) = sum(sum((Ytest - Dtest).^2))/(numTest*numClasses) + regCof*(regW + regV);
    cmTr = calcConfusionMatrix( lblTrain, Lt{1});
    cmTe = calcConfusionMatrix( lblTest, Lt{2});
    trainingAcc(1+n) = calcAccuracy(cmTr);
    testAcc(1+n) = calcAccuracy(cmTe);
end

figure(101)
clf
[mErrTr, mErrIndTr] = max(trainingAcc);
[mErr, mErrInd] = max(testAcc);
plot(trainingAcc,'k','linewidth',1.5)
hold on
plot(testAcc,'r','linewidth',1.5)
plot(mErrInd,mErr,'bo','linewidth',1.5)
plot(mErrIndTr,mErrTr,'bo','linewidth',1.5)
hold off
title('Training and Test Accuracy, Multi-Layer')
legend('Training Accuracy','Test Accuracy','Max Test Accuracy','Max Training Accuracy')

n
display(['Max Training Accuracy: ' num2str(max(trainingAcc))])
display(['Max Test Accuracy: ' num2str(max(testAcc))])
end
