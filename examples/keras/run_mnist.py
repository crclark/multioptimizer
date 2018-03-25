import numpy as np
from tensorflow.python.keras.layers import Input, Dense
from tensorflow.python.keras.models import Model, model_from_json
from tensorflow.python.keras import regularizers
from sklearn.model_selection import StratifiedKFold
from sklearn.datasets import fetch_mldata
from sklearn.metrics import classification_report
from sklearn.preprocessing import OneHotEncoder
import argparse

parser = argparse.ArgumentParser(description='Process some integers.')
parser.add_argument('model',
                    help='path to JSON model')
args = parser.parse_args()

mnist = fetch_mldata('MNIST original')
data = mnist.data
n_samples = len(data)
ohc = OneHotEncoder(sparse=False)
labels = ohc.fit_transform(mnist.target.reshape(-1, 1))
n_labels = 10

def mk_model():
  x = Dense(64, activation='relu')(inputs)
  x = Dense(64, activation='relu', kernel_regularizer=regularizers.l2(0.01))(x)
  predictions = Dense(10, activation='softmax')(x)
  model = Model(inputs=inputs, outputs=predictions)
  with open("debugmodel.json", 'w') as f:
    f.write(model.to_json())
  return model

def load_model(filepath):
  with open(filepath,'r') as f:
    return model_from_json(f.read())

def accuracy_vec(real, predicted):
  counts = np.array([0]*n_labels)
  n_correct = np.array([0]*n_labels)
  for pred_ix in range(len(predicted)):
    pred_class = np.argmax(predicted[pred_ix])
    real_class = np.argmax(real[pred_ix])
    counts[real_class] += 1
    if pred_class == real_class:
      n_correct[real_class] += 1
  return n_correct / counts

def fit_and_eval_model(model, data_train, labels_train, data_test, labels_test):
  # TODO: more epochs + early stopping
  model.fit(data_train, labels_train, epochs=2, verbose=0)
  predicted = model.predict(data_test)
  return accuracy_vec(labels_test, predicted)

inputs = Input(shape=(784,))
# TODO: mcts can already handle noisy objectives. Just use train/test split.
n_folds = 3
skf = StratifiedKFold(n_splits=n_folds, shuffle=True)

results = np.array([0.0]*n_labels)

for (train_indices, test_indices) in skf.split(data, mnist.target.reshape(-1,1)):
  model = load_model(args.model)
  model.compile(optimizer='rmsprop',
                loss='categorical_crossentropy',
                metrics=['accuracy'])
  data_train, data_test = data[train_indices], data[test_indices]
  labels_train, labels_test = labels[train_indices], labels[test_indices]
  results += fit_and_eval_model(model, data_train, labels_train, data_test, labels_test)

averaged_result = results / n_folds
for i in range(n_labels):
  print(averaged_result[i])
