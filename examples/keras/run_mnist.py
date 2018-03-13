from tensorflow.python.keras.layers import Input, Dense
from tensorflow.python.keras.models import Model, model_from_json
from tensorflow.python.keras import regularizers
from sklearn.datasets import fetch_mldata
from sklearn.preprocessing import OneHotEncoder

mnist = fetch_mldata('MNIST original')
data = mnist.data
one_hot = OneHotEncoder(sparse=False)
labels = one_hot.fit_transform(mnist.target.reshape(-1, 1))

print(data.shape)
print(labels.shape)

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

# This returns a tensor
inputs = Input(shape=(784,))

# a layer instance is callable on a tensor, and returns a tensor

# This creates a model that includes
# the Input layer and three Dense layers
model = load_model("test_output.json")
model.compile(optimizer='rmsprop',
              loss='categorical_crossentropy',
              metrics=['accuracy'])
model.fit(data, labels)  # starts training

