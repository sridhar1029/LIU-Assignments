function predictions = predict_knn(model, new_data)
dis = pdist2(model.X, new_data);
[~, i] = mink(dis, model.k);
preds = model.Y(i);
preds = preds + 1;
dim = size(new_data);
dim = dim(1);
res = zeros(10, dim);
for i = 1:dim
    for j = 1:model.k
        cl = preds(j, i);
        res(cl, i) = res(cl, i) + 1;
    end
end
[~, predictions] = max(res);
predictions = predictions - 1;
predictions = predictions';
end