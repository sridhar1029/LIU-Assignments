function [ labelsOut ] = kNN(X, k, Xt, Lt)
%KNN Your implementation of the kNN algorithm
%   Inputs:
%               X  - Features to be classified
%               k  - Number of neighbors
%               Xt - Training features
%               LT - Correct labels of each feature vector [1 2 ...]'
%
%   Output:
%               LabelsOut = Vector with the classified labels

%labelsOut  = zeros(size(X,2),1);
X = X';
Xt = Xt';
classes = unique(Lt);
numClasses = length(classes);

dis = pdist2(Xt, X);
[~, i] = mink(dis, k);
preds = Lt(i);
if k>1
    dim = size(X, 1);
    res = zeros(numClasses, dim);
    for i = 1:dim
        for j = 1:k
            cl = preds(j, i);
            res(cl, i) = res(cl, i) + 1;
        end
    end
    [~, labelsOut] = max(res);
    labelsOut = labelsOut';
else
    labelsOut = preds;
end
end

