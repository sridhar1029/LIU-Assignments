function acc = accuracy(preds, true_vals)
correct = sum(preds == true_vals);
dims = size(preds);
n = dims(1);
acc = correct/n;
end

