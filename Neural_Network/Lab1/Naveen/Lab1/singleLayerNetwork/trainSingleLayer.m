function [Wout, trainingError, testError ] = trainSingleLayer(X_t,D_t,Xtest,Dtest, W0,numIterations, learningRate )
%TRAINSINGLELAYER Trains the network (Learning)
%   Inputs:
%               X* - Trainin/test features (matrix)
%               D* - Training/test desired output of net (matrix)
%               W0 - Weights of the neurons (matrix)
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
Nt = size(X_t,2);                          %Number of training examples;
Ntest = size(Xtest,2);                     %Number of test cases; 
Wout = W0;                                 %Weight of dimension 2*3

% Calculate initial error
Yt = runSingleLayer(X_t, W0);               %Run single layer with train data and initialized weights
Ytest = runSingleLayer(Xtest, W0);
trainingError(1) = sum(sum((Yt - D_t).^2))/Nt;
testError(1) = sum(sum((Ytest - Dtest).^2))/Ntest;

for n = 1:numIterations
    Wout_old = Wout;
    Y = runSingleLayer(X_t,Wout); 
    grad_w = (2*(Y-D_t)/Nt)*X_t';
    Wout = Wout - learningRate*grad_w;
    trainingError(1+n) = sum(sum((Wout*X_t - D_t).^2))/Nt;
    testError(1+n) = sum(sum((Wout*Xtest - Dtest).^2))/Ntest;
     if(testError(n)-testError(n+1) < 0)
        Wout = Wout_old;
        trainingError(1+n)= NaN;
        testError(1+n) = NaN;
        break
    end                               %break when test error is lesser than 0.004    
end

trainingError = trainingError(1:n);
testError = testError(1:n);   %Send back  only speciefied number of train and test error
end

