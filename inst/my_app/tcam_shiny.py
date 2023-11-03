import pandas as pd
import numpy as np
from mprod import table2tensor
from mprod.dimensionality_reduction import TCAM


def tca_all(file):
    data_table = file
    data_table = data_table.set_index(['id', 'time'])
    data_table = data_table
    
    data_tensor, map1, map3 =  table2tensor(data_table, missing_flag=False)
    tca = TCAM()
    
    # tca_varexplained
    tca_trans = tca.fit_transform(data_tensor)
    tca_var = tca.explained_variance_ratio_*100 # % explained variation per TCA factor
    
    # tca 
    tca_df = pd.DataFrame(tca_trans)   # Cast TCA scores to dataframe
    tca_df.rename(index = dict(map(reversed, map1.items())), inplace = True)   
                
    # tca_loadings
    tca_trans = tca.fit_transform(data_tensor)
    tca_loadings = tca.mode2_loadings  
    
    return(tca_df, tca_loadings, tca_var)

