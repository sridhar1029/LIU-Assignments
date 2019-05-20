function [ acc ] = calcAccuracy( cM )
%CALCACCURACY Takes a confusion matrix amd calculates the accuracy

acc = 0; % Replace with your own code
numCl = size(cM, 1);
crr = 0;
tot = sum(cM(:));
for i = 1:numCl
    crr = crr + cM(i, i);
end
acc = crr/tot;
end

