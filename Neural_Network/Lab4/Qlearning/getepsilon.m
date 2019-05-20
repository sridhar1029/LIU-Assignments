function epsilon = getepsilon(episode, decay)
% GETEPSILON Function for getting epsilon (exploration factor) given several
% parameters, e.g. the current and total number of episodes.
% You may choose not to use this function but it might be helpful if you
% would like to plot how epsilon changes during the training. If you choose
% to use this function you are free to add or remove any parameters.
%
% Implement your own epsilon-function here if you want to.
max_exploration_rate = 1;
min_exploration_rate = 0.1;
epsilon = min_exploration_rate + ...
        (max_exploration_rate - min_exploration_rate)*exp(-decay*episode);
end

