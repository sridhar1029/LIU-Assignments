function [ labelsOut ] = kNN(X, k, X_t, L_t)
%KNN Your implementation of the kNN algorithm
%   Inputs:
%               X  - Features to be classified
%               k  - Number of neighbors
%               Xt - Training features
%               LT - Correct labels of each feature vector [1 2 ...]'
%
%   Output:
%   LabelsOut = Vector with the classified labels

%Xt{2}, k, Xt{1}, Lt{1

labelsOut  = zeros(size(X,2),1);  % Labels of test data
%classes = unique(L_new);                % How many classes we need, not using      
%numClasses = length(classes);           % Length of classes, not using

dist = zeros(1,length(X_t));     

for i =1:length(X)
   dist = pdist2(X(:,i)',X_t');
   [~,ind] = mink(dist,k);  % Find k minimum distances
   y =  L_t(ind);             % find the labels of k nearest label
   labelsOut(i) = mode(y);
end

end


