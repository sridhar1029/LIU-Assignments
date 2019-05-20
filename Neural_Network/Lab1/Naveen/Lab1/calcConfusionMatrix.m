function [ cM ] = calcConfusionMatrix( Lclass, Ltrue )
classes = unique(Ltrue);
numClasses = length(classes);
cM = zeros(numClasses);

% Add your own code here

for i = 1:numClasses
    allclass = Lclass(Ltrue == classes(i));
    for j = 1:numClasses
        cM(i,j) = sum(allclass==classes(j));
    end
end

end

 

