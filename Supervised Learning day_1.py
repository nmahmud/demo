
# coding: utf-8

# In[2]:


import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from sklearn import datasets
from sklearn import tree
iris = datasets.load_iris() #load the Data


# In[13]:


print(iris.target_names)

# Make a dataframe for easier use later
X = iris.data
y = iris.target


# In[20]:


# Make X and y a DataFrame
df_x = pd.DataFrame(X, columns=iris.feature_names)
df_y = pd.DataFrame(y,columns=['Type of Flower'])
df = pd.concat([df_x,df_y],axis=1)
df.head()


# Visualise the data

# In[21]:


# Plotting using a scatter plot, first column by second column, 
# c = y tells python to color the nodes based on their classification.

# Quickest graph to Visualize
#plt.style.use('ggplot') # ggplot from R

plot = plt.scatter(df.iloc[:,0], df.iloc[:,1], c = df['Type_Flower'])
plt.title('Sepal length by Sepal Width colored by flower')
plt.xlabel('Sepal Length')
plt.ylabel('Sepal Width')
plt.show()
plt.close()


# In[23]:



from mpl_toolkits.mplot3d import Axes3D

# Fancy 3D Plot
fig = plt.figure(1, figsize=(8, 6))
ax = Axes3D(fig, elev=-178, azim=50)

ax.scatter(df.iloc[:, 0], df.iloc[:, 1], df.iloc[:, 2], c=df.iloc[:,-1], cmap=plt.cm.Set1, edgecolor='k', s=40)
ax.set_title("3D Representation of Sepal Width, Sepal Length, and Petal Length")
ax.set_xlabel('Sepal Length')
ax.set_ylabel('Sepal Width')
ax.set_zlabel('Petal Length')

plt.show()
plt.close()
fig.clf()


# Although it will be easy to seperate the data linearly for red data points but it will be difficult to seperate the orange and grey points.

# In[25]:


# Splitting the data into Train-test set
from sklearn.model_selection import train_test_split
X = df.iloc[:,:-1]
y = df.iloc[:,-1]
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3)
assert len(X_train)+ len(X_test) == len(X)


# In[1]:


from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import classification_report

# Initialize call to ML Algorithm of choice
knn = KNeighborsClassifier(n_neighbors = 6)

# Fit the data
knn = knn.fit(X_train, y_train)

# Predict on the data
y_prediction = knn.predict(X_test)

# Classification Report of predictions
print(classification_report(y_test, y_prediction))

