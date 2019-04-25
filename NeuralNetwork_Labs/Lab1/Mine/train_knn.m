function [model, acc_mat] = train_knn
[model, val] = split_data;
model.k = 2;
tests = 40;
acc_mat = zeros(tests, 3);
for i = 2:tests
    model.k = i;
    preds = predict_knn(model, model.X);
    tr_acc = accuracy(preds, model.Y);
    preds = predict_knn(model, val.X_val);
    val_acc = accuracy(preds, val.Y_val);
    acc_mat(i, 1) = model.k;
    acc_mat(i, 2) = tr_acc;
    acc_mat(i, 3) = val_acc;
end
[~, best_k] = max(acc_mat(:, 3));
model.k = best_k;

plot(acc_mat(2:end,1), acc_mat(2:end, 2));
hold on;
plot(acc_mat(2:end,1), acc_mat(2:end, 3));
xlabel('Number of Neighbours : K');
ylabel('Accuracy')
title('Train and Validation accuracy for different values of K')
legend('Train accuracy', 'Validation Accuracy')
end

