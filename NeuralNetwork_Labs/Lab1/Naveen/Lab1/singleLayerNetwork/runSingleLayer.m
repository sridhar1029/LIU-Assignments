function [ Y, L ] = runSingleLayer(X, W)
%EVALUATESINGLELAYER Summary of this function goes here
%   Inputs:
%               X  - Features to be classified (matrix)
%               W  - Weights of the neurons (matrix)
%
%   Output:
%               Y = Output for each feature, (matrix)
%               L = The resulting label of each feature, (vector) 

%weight matrix is 2*3
%Input matrix X is 3*200

Y = W*X;

% Calculate classified labels
[~, L] = max(Y,[],1);     % Returns the largest elements along dimension 2 
L = L(:);
end

