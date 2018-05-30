import numpy as np
import tensorflow as tf

import matplotlib.pyplot as plt

import tensorflow.contrib.eager as tfe


tf.enable_eager_execution()


DATASET_SIZE = int(10e2)
GRAD, BIAS, NOISE = 2.7, 1.9, 0.1
BATCH_SIZE, LEARNING_RATE, EPOCHS = 32, 1.e-1, 10

x_data = np.random.uniform(size=[DATASET_SIZE, 1]).astype(np.float32)

y_data = BIAS + GRAD * x_data + np.random.normal(size=x_data.shape, scale=NOISE)
y_data = y_data.astype(np.float32)

plt.scatter(x_data, y_data, label='data')

dataset = tf.data.Dataset.from_tensor_slices((x_data, y_data))
dataset = dataset.repeat(EPOCHS)
dataset = dataset.batch(BATCH_SIZE)


class LinearRegression:
    def __init__(self):
        self.w = tf.get_variable('w', shape=())
        self.b = tf.get_variable('b', shape=())

        self.variables = self.w, self.b

    def predict(self, inputs):
        return self.b + inputs * self.w

    def __call__(self, *args, **kwargs):
        return self.predict(*args, **kwargs)


def loss(model, inputs, targets):
    prediction = model(inputs)
    return tf.losses.mean_squared_error(labels=targets, predictions=prediction)


def grad(model, inputs, targets):
    with tfe.GradientTape() as tape:
        loss_value = loss(model, inputs, targets)

    return tape.gradient(loss_value, model.variables)


linear_regression = LinearRegression()
optimizer = tf.train.AdamOptimizer(learning_rate=LEARNING_RATE)

print("Initial loss: {:.3f}".format(
    loss(linear_regression.predict, x_data, y_data)))

for i, (x, y) in enumerate(tfe.Iterator(dataset)):
    grads = grad(linear_regression, x, y)

    optimizer.apply_gradients(
        zip(grads, linear_regression.variables),
        global_step=tf.train.get_or_create_global_step())

    if i % 20 == 0:
        print("Loss at step {:03d}: {:.3f}".format(
            i, loss(linear_regression, x_data, y_data)))

print("Final loss: {:.3f}".format(
    loss(linear_regression.predict, x_data, y_data)))

predictions_x = np.linspace(min(x_data), max(x_data), num=1000).reshape([-1, 1])
predictions_y = linear_regression(predictions_x)

plt.scatter(predictions_x, predictions_y.numpy(), label='predictions')
plt.legend()
