"""
Run this script to clean the initial crabs
dataset and create a cleaned version of it
under the data folder.
"""


import pandas as pd
from csv import reader

def preprocess_crabs(output_path='data/crabs_preprocessed.csv'):
    with open('data/crab.csv','r') as file:
        text = file.readlines()
    col_names = ['index','color','spine_condition','weight','carapace_width', 'satellites']
    text_preprocessed = map(lambda line: ','.join(line.split()),text)
    _df = pd.DataFrame( list(reader(text_preprocessed)),columns=col_names)
    _df = _df.drop(columns='index')
    _df.to_csv(output_path,index=False)
    return True


if __name__ == '__main__':
    try:
            preprocess_crabs()
        print('Dataset Successfully preprocessed and saved in \'data\' folder.')
    except Exception as e:
        print(e)
