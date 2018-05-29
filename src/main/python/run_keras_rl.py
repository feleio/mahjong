import numpy as np

from keras.models import Sequential
from keras.layers import Dense, Activation, Flatten
from keras.optimizers import Adam

from rl.agents.dqn import DQNAgent
from rl.core import Env, Space
from rl.policy import BoltzmannQPolicy
from rl.memory import SequentialMemory

import random

class MyObSpace(Space):
    def sample(self, seed=None):
        return np.array([random.randint(0, 3), random.randint(0, 3), random.randint(0, 3)])

    def contains(self, x):
        return all(y in {0,1,2,3,4,5,6,9,10} for y in x)

class MyAcSpace(Space):
    def sample(self, seed=None):
        return np.array(random.choice([[0, 0, 1], [0, 1, 0], [1,0,0]]))

    def contains(self, x):
        return x in [[0, 0, 1], [0, 1, 0], [1,0,0]]


class MyEnv(Env):
    def __init__(self):
        self.observation_space = MyObSpace()
        self.action_space = MyAcSpace()
        self.st = np.zeros(3)
        self.step_count = 0

    def step(self, action):
        self.step_count +=1
        self.st[action] +=1
        print(self.st, action)
        return (self.st,
                20-self.step_count if self.st[0] + self.st[1]*2 == self.st[2] else -1,
                (self.st[0] + self.st[1]*2 == self.st[2]) or sum(self.st) > 20,
                {1:1})
        # # return observation, reward, done, info
        # if action[0] > 0:
        #     action
        # return [1,0], reward, done, {1:1}
    def reset(self):
        self.st = self.observation_space.sample()
        self.step_count = 0
        return self.st

    def render(self, *a, **k):
        print(self.st, a, k)
    def close(self):
        pass

env = MyEnv()
# np.random.seed(123)
# env.seed(123)
nb_actions = 3#env.action_space.n

# Next, we build a very simple model.
model = Sequential()
model.add(Flatten(input_shape=(1,) + (3,)))
model.add(Dense(16))
model.add(Activation('relu'))
model.add(Dense(16))
model.add(Activation('relu'))
model.add(Dense(16))
model.add(Activation('relu'))
model.add(Dense(nb_actions))
model.add(Activation('linear'))
print(model.summary())

# Finally, we configure and compile our agent. You can use every built-in Keras optimizer and
# even the metrics!
memory = SequentialMemory(limit=50000, window_length=1)
policy = BoltzmannQPolicy()
dqn = DQNAgent(model=model, nb_actions=nb_actions, memory=memory, nb_steps_warmup=1,
               target_model_update=1e-2, policy=policy)
dqn.compile(Adam(lr=1e-3), metrics=['mae'])

# Okay, now it's time to learn something! We visualize the training here for show, but this
# slows down training quite a lot. You can always safely abort the training prematurely using
# Ctrl + C.
history = dqn.fit(env, nb_steps=5000, visualize=True, verbose=2)

# After training is done, we save the final weights.
# dqn.save_weights('dqn_{}_weights.h5f'.format(ENV_NAME), overwrite=True)

# Finally, evaluate our algorithm for 5 episodes.
dqn.test(env, nb_episodes=5, visualize=True)