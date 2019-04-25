function [ Y, L ] = runMultiLayer( X, W, V )
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
U = tanh(S); %Calculate the activation function as a hyperbolic tangent
dim_u = size(U, 2);
U_b = [ones(1, dim_u); U];
Y = V*U_b; %Calculate the sumation of the output neuron
   

% Calculate classified labels
[~, L] = max(Y,[],1);
L = L(:);

end

