function [ cM ] = calcConfusionMatrix( Lclass, Ltrue )
classes = unique(Ltrue);
numClasses = length(classes);
cM = zeros(numClasses);

% Add your own code here
for i = 1:numClasses
    cM(i, i) = sum((Lclass == i).*(Ltrue == i));
    tl_fc = Lclass(Ltrue == i);
    for j = 1:numClasses
        if j~=i
            cM(i, j) = sum(tl_fc==j);
        end
    end
end
end

