%% Initialization
%  Initialize the world, Q-table, and hyperparameters

test = 0

%parameters gw1 (Annoying blob)
gw = 1
num_episodes = 2000;
max_steps_each_episode = 500;

learning_rate = 0.2;
discount_rate = 0.9;

exploration_rate = 1;
exploration_rate_decay = 0.001;

%parameters gw2 (Stochastic annoying blob)
% gw = 2
% num_episodes = 3000;
% max_steps_each_episode = 500;
% 
% learning_rate = 0.2;
% discount_rate = 0.9;
% 
% exploration_rate = 1;
% exploration_rate_decay = 0.001;

% %parameters gw3 (The road to HG)
% gw = 3
% num_episodes = 3000;
% max_steps_each_episode = 1000;
% 
% learning_rate = 0.4;
% discount_rate = 0.9;
% 
% exploration_rate = 1;
% exploration_rate_decay = 0.001;

%parameters gw4 (The road to HG random moves)
% gw = 4
% num_episodes = 10000;
% max_steps_each_episode = 500;
% 
% learning_rate = 0.1;
% discount_rate = 0.9;
% 
% exploration_rate = 1;
% exploration_rate_decay = 0.001;

%initializations for training
gwinit(gw);
s = gwstate;
action_space = [1,2,3,4];
action_space_prob = [1/4, 1/4, 1/4, 1/4];
action_space_size = 4;
sizeX = s.xsize;
sizeY = s.ysize;
q_table = zeros(sizeY, sizeX, action_space_size);

counts_fin = 0;
rewards_all_episodes = zeros(1, num_episodes);

%% Training loop
%  Train the agent using the Q-learning algorithm.
for episode = 1:num_episodes
    gwinit(gw);
    state = gwstate;
    
    done = 0;
    rewards_cur_episode = 0;
    
    for step = 1:max_steps_each_episode
        r = unifrnd(0,1);
        cur_pos = state.pos;
        if r > exploration_rate
            [val, action] = max(q_table(cur_pos(1), cur_pos(2), :));
        else
            action = sample(action_space, action_space_prob);
        end
        
        new_state = gwaction(action);
        %During the training of the forth world we will have to remove this 
        %negative q value for invalid moves
        if ~new_state.isvalid
            q_table(cur_pos(1), cur_pos(2), action) = -1000;
        else
            done = new_state.isterminal;
            new_pos = new_state.pos;
            q_table(cur_pos(1), cur_pos(2), action) = (1-learning_rate)*q_table(cur_pos(1), cur_pos(2), action) + ...
                learning_rate*(new_state.feedback + discount_rate*max(q_table(new_pos(1), new_pos(2), :)));
        end
        state = new_state;
        rewards_cur_episode = rewards_cur_episode + new_state.feedback;
        if done
            counts_fin = counts_fin + 1;
            break
        end
    end
    
    %exploration rate decay
    exploration_rate = getepsilon(episode, exploration_rate_decay);
    
    rewards_all_episodes(episode) = rewards_cur_episode;
    if mod(episode, 1000)==0
        episode
    end
end

%% Test loop
%  Test the agent (subjectively) by letting it use the optimal policy
%  to traverse the gridworld. Do not update the Q-table when testing.
%  Also, you should not explore when testing, i.e. epsilon=0, always pick
%  the optimal action.
%rew_start = 1;
%rew_step = 1000;
%for i= 1:10
%    rew_end = rew_start + rew_step - 1;
%    avg = sum(rewards_all_episodes(rew_start:rew_end))/rew_step
%    rew_start = rew_end + 1;
%end
%gw = 4
% if gw==1
%     load gw1;
% elseif gw==2
%     load gw2;
% elseif gw==3
%     load gw3;
% elseif gw==4
%     load gw4;
% end
figure(1)
rewards_test_episode = 0;
gwinit(gw);
state = gwstate;
gwdraw
for step = 1:1000
    cur_pos = state.pos;
    [val, action] = max(q_table(cur_pos(1), cur_pos(2), :));
    gwplotarrow(state.pos, action)

    state = gwaction(action);
    
    rewards_test_episode = rewards_test_episode + state.feedback;
    if state.isterminal
        fprintf("Success!! Found the goal.")
        break
    end
end
rewards_test_episode


%% Check success rate of the algorithm
if test == 1
    rng(123456789)
    num_test_episodes = 100;
    num_success = 0;
    for i = 1:num_test_episodes
        gwinit(gw);
        state = gwstate;
        for step = 1:1000
            cur_pos = state.pos;
            [val, action] = max(q_table(cur_pos(1), cur_pos(2), :));

            state = gwaction(action);

            rewards_test_episode = rewards_test_episode + state.feedback;
            if state.isterminal
                num_success = num_success + 1;
                break
            end
        end
    end
    num_success
end



%% Get best policy
%gw = 4
figure(2)
gwinit(gw)
% if gw==1
%     load gw1;
% elseif gw==2
%     load gw2;
% elseif gw==3
%     load gw3;
% elseif gw==4
%     load gw4;
% end
gwdraw(q_table)

%% plot exploration rate
% eps_plot = zeros(4, 5000);
% for i = 1:5000
%     eps_plot(1, i) = getepsilon(i, 0.1);
%     eps_plot(2, i) = getepsilon(i, 0.01);
%     eps_plot(3, i) = getepsilon(i, 0.001);
%     eps_plot(4, i) = getepsilon(i, 0.0001);
% end
% plot(eps_plot(1, :),'LineWidth',2)
% hold on;
% plot(eps_plot(2, :),'LineWidth',2)
% plot(eps_plot(3, :),'LineWidth',2)
% plot(eps_plot(4, :),'LineWidth',2)
% legend('Decay : 0.1', 'Decay : 0.01', 'Decay : 0.001', 'Decay : 0.0001')
% xlabel("Episodes")
% ylabel("Exploration Rate")
% title("Plot showing exploration rate vs episode for different decay rates")
% hold off;