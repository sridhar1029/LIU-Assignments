%% Hyper-parameters
%  You will need to change these. Start with a small number and increase
%  when your algorithm is working.
clear;
clc;
format compact;

% Number of randomized Haar-features
nbrHaarFeatures = 50;
% Number of training images, will be evenly split between faces and
% non-faces. (Should be even.)
nbrTrainImages = 1000;
% Number of weak classifiers
nbrWeakClassifiers = 100;

%% Load face and non-face data and plot a few examples
%  Note that the data sets are shuffled each time you run the script.
%  This is to prevent a solution that is tailored to specific images.

load faces;
load nonfaces;
faces = double(faces(:,:,randperm(size(faces,3))));
nonfaces = double(nonfaces(:,:,randperm(size(nonfaces,3))));

figure(1);
colormap gray;
for k=1:25
    subplot(5,5,k), imagesc(faces(:,:,10*k));
    axis image;
    axis off;
end

figure(2);
colormap gray;
for k=1:25
    subplot(5,5,k), imagesc(nonfaces(:,:,10*k));
    axis image;
    axis off;
end

%% Generate Haar feature masks
haarFeatureMasks = GenerateHaarFeatureMasks(nbrHaarFeatures);

figure(3);
colormap gray;
for k = 1:25
    subplot(5,5,k),imagesc(haarFeatureMasks(:,:,k),[-1 2]);
    axis image;
    axis off;
end

%% Create image sets (do NOT modify!)

% Create a training data set with examples from both classes.
% Non-faces = class label y=-1, faces = class label y=1
trainImages = cat(3,faces(:,:,1:nbrTrainImages/2),nonfaces(:,:,1:nbrTrainImages/2));
xTrain = ExtractHaarFeatures(trainImages,haarFeatureMasks);
yTrain = [ones(1,nbrTrainImages/2), -ones(1,nbrTrainImages/2)];

% Create a test data set, using the rest of the faces and non-faces.
testImages  = cat(3,faces(:,:,(nbrTrainImages/2+1):end),...
                    nonfaces(:,:,(nbrTrainImages/2+1):end));
xTest = ExtractHaarFeatures(testImages,haarFeatureMasks);
yTest = [ones(1,size(faces,3)-nbrTrainImages/2), -ones(1,size(nonfaces,3)-nbrTrainImages/2)];

% Variable for the number of test-data.
nbrTestImages = length(yTest);

%% Implement the AdaBoost training here
%  Use your implementation of WeakClassifier and WeakClassifierError
best_classifiers = zeros(nbrWeakClassifiers, 4);
D = ones(1, nbrTrainImages)./nbrTrainImages;
D_test = ones(1, nbrTestImages)./nbrTestImages;
train_error = zeros(nbrWeakClassifiers, 1);
test_error = zeros(nbrWeakClassifiers, 1);
train_acc = zeros(nbrWeakClassifiers, 1);
test_acc = zeros(nbrWeakClassifiers, 1);
for j = 1:nbrWeakClassifiers
    err_mat1 = zeros(nbrHaarFeatures, nbrTrainImages);
    err_mat2 = zeros(nbrHaarFeatures, nbrTrainImages);
    for i = 1:nbrTrainImages
        C = WeakClassifier(xTrain(:,i), ones(nbrHaarFeatures, 1), xTrain);
        err1 = WeakClassifierError(C, D, yTrain);
        err_mat1(:, i) = err1;
        C = WeakClassifier(xTrain(:,i), (ones(nbrHaarFeatures, 1).*-1), xTrain);
        err2 = WeakClassifierError(C, D, yTrain);
        err_mat2(:, i) = err2;
    end
    [vals1, inds1] = min(err_mat1, [], 2);
    [vals2, inds2] = min(err_mat2, [], 2);
    inds = [inds1, inds2];
    vals = [vals1, vals2];
    [val_row,ind_row] = min(vals);
    [val_col,ind_col] = min(val_row);
    best_classifiers(j, 1) = ind_row(ind_col);
    thresh_ind = inds(ind_row(ind_col), ind_col);
    best_classifiers(j, 2) = xTrain(best_classifiers(j, 1), thresh_ind);
    if ind_col == 1
        best_classifiers(j, 3) = 1;
    else
        best_classifiers(j, 3) = -1;
    end
    %try updating using the strong classifier insted of the weak classifier
    C = WeakClassifier(best_classifiers(j, 2), best_classifiers(j, 3), xTrain(best_classifiers(j, 1), :));
    %best_classifiers(j, 4) = 1;
    %[best_err, acc, C] = StrongClassifier(X(best_classifiers(1:j, 1), :), Y, best_classifiers(1:j,:), D(1,x_inds));
    best_err = WeakClassifierError(C, D, yTrain);
    best_classifiers(j, 4) = (1/2)*log((1-best_err)/best_err);
    D(yTrain~=C) = D(yTrain~=C)*exp(best_classifiers(j, 4));
    D(yTrain==C) = D(yTrain==C)*exp(-best_classifiers(j, 4));
    D = D./sum(D);
    acc = sum(yTrain==C)/nbrTrainImages;
    fprintf("%d     %d    %d\n", j, best_err, acc)
    
    [err, acc, ~] = StrongClassifier(xTrain(best_classifiers(1:j, 1), :), yTrain, best_classifiers(1:j,:), ones(1, nbrTrainImages)./nbrTrainImages);
    train_error(j) = err;
    train_acc(j) = acc;
    [err, acc] = StrongClassifier(xTest(best_classifiers(1:j, 1), :), yTest, best_classifiers(1:j,:), D_test);
    test_error(j) = err;
    test_acc(j) = acc;
end

%% Evaluate your strong classifier here
%  You can evaluate on the training data if you want, but you CANNOT use
%  this as a performance metric since it is biased. You MUST use the test
%  data to truly evaluate the strong classifier.
[err, acc, C] = StrongClassifier(xTest(best_classifiers(:, 1), :), yTest, best_classifiers, D_test);
fprintf("Strong classifier on Test data\nError : %f\nAccuracy : %f\n", err, acc)
max(test_acc)
min(test_error)

%% Plot the error of the strong classifier as a function of the number of weak classifiers.
%  Note: you can find this error without re-training with a different
%  number of weak classifiers.
figure(4)
clf
[mAccTr, mAccIndTr] = max(train_acc);
[mAcc, mAccInd] = max(test_acc);
plot(train_acc,'k','linewidth',1.5)
hold on
plot(test_acc,'r','linewidth',1.5)
plot(mAccInd,mAcc,'bo','linewidth',1.5)
plot(mAccIndTr,mAccTr,'bo','linewidth',1.5)
hold off
title('Training and Test Accuracy, Ada-Boost')
legend('Training Accuracy','Test Accuracy','Max Test Accuracy','Max Training Accuracy')

figure(5)
clf
[mErrTr, mErrIndTr] = min(train_error);
[mErr, mErrInd] = min(test_error);
plot(train_error,'k','linewidth',1.5)
hold on
plot(test_error,'r','linewidth',1.5)
plot(mErrInd,mErr,'bo','linewidth',1.5)
plot(mErrIndTr,mErrTr,'bo','linewidth',1.5)
hold off
title('Training and Test Errors, Ada-Boost')
legend('Training Error','Test Error','Max Test Error','Max Training Error')



%% Plot some of the misclassified faces and non-faces from the test set
%  Use the subplot command to make nice figures with multiple images.
misc_inds = 1:nbrTestImages;
misc_inds = misc_inds(C ~= yTest);
tot_faces = sum(yTest==1);
misc_faces = misc_inds(misc_inds<=tot_faces) + nbrTrainImages/2;
misc_nonfc = misc_inds(misc_inds>tot_faces) - tot_faces + nbrTrainImages/2;

figure(6);
colormap gray;
for k=1:25
    ind = misc_faces(k);
    subplot(5,5,k), imagesc(faces(:,:,ind));
    axis image;
    axis off;
end

figure(7);
colormap gray;
for k=1:25
    ind = misc_nonfc(k);
    subplot(5,5,k), imagesc(nonfaces(:,:,ind));
    axis image;
    axis off;
end




%% Plot your choosen Haar-features
%  Use the subplot command to make nice figures with multiple images.
figure(8);
colormap gray;
chosenHaars = unique(best_classifiers(:,1));
for k = 1:length(chosenHaars)
    fe = chosenHaars(k);
    subplot(9,5,k),imagesc(haarFeatureMasks(:,:,fe),[-1 2]);
    axis image;
    axis off;
end
