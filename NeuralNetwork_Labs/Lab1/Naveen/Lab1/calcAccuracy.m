function [ acc ] = calcAccuracy( cM )
%CALCACCURACY Takes a confusion matrix amd calculates the accuracy

dim = size(cM,1);
acc=0;

for i= 1:dim
    acc=cM(i,i)+ acc;
end

acc = acc/sum(sum(cM));

end

