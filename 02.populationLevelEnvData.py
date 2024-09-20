#katherine carbeck

## extract env data for the populations
# envfile = POPULATION_ClimateNA_output_el_withCollectionDates.csv

#! bash
export PYTHONPATH=/home/kcarbeck/.local/lib64/python3.9/site-packages:$PYTHONPATH
python
#! 

import pandas as pd

env = pd.read_csv("/workdir/kcarbeck/data/envdir/envfile_sub.csv")
env.set_index('Population', inplace=True)

# save full dataframe with the index as the population column
env.to_csv("/workdir/kcarbeck/data/envdir/envfile.txt")
