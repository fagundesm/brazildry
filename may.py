import pandas as pd
from scipy.spatial.distance import pdist, squareform

data = pd.read_csv('dadosmai.csv')

dist_matrix = []
for i in range(0,len(data['plot'].unique())):
    distances = pdist(data.iloc[list(data[data['plot']==[data['plot'].unique()[i]]].index),3:].values, metric='euclidean')
    dist_matrix.append(squareform(distances))
