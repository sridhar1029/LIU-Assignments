function [ Y,L] = runMultiLayer( X, W, V )
%RUNMULTILAYER Calculates output and labels of the net
%   Inputs:
%               X  - Features to be classified (matrix)
%               W  - Weights of the hidden neurons (matrix)
%               V  - Weights of the output neurons (matrix)
%
%   Output:
%               Y = Output for each feature, (matrix)
%               L = The resulting label of each feature, (vector) 

S = W*X; %Calculate the sumation of the weights and the input signals (hidden neuron)
U = [ones(1,size(X,2));tanh(S)]; %Add a bias 1 to make it *number of inputs to match the weights of output neurons
Y = V*U; %Output of last layer
   

% Calculate classified labels
[~, L] = max(Y,[],1);
L = L(:);

end

