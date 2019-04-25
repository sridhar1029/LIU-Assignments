function [err, acc, lbls] = StrongClassifier(X, Y, classifiers, D)
%STRONGCLASSIFIER Summary of this function goes here
%   Detailed explanation goes here
preds = WeakClassifier(classifiers(:, 2), classifiers(:, 3), X);
lbls = sum(preds.*classifiers(:, 4), 1);
lbls = (lbls>=0).*1 + (lbls<0).*-1;
err = WeakClassifierError(lbls, D, Y);
crr = sum(lbls == Y);
acc = crr / size(Y, 2);
end

