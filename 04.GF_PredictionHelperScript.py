# use '04.gradient_prediction_script.R' script to predict GF models
# katherine carbeck
# 13 august 2024


###*############################################*###
         ###* 2. PREDICT FOR EACH YEAR *###
###*############################################*###
##! python
python
from pythonimports import *
import subprocess
import concurrent.futures

#see imported modules:
import sys
for module_name in sys.modules:
     print(module_name)



DIR = '/workdir/kcarbeck'

# define the range of years to iterate over
years = range(1971, 2023 + 1)

predict_script_file = f'{DIR}/04.gradient_prediction_script.R'  
save_dir = f'{DIR}/data/predictions'
model_dir = f'{DIR}/data/training' 
imports_path = f'{DIR}/r_imports'


def run_prediction(year):
    range_file = f'{DIR}/data/rasterdir/RasterStackTxtFiles/raster_data_{year}.txt'
    basename = f'out_{year}'
    
    predict_command = [
        'Rscript',
        predict_script_file,
        range_file,
        basename,
        save_dir,
        model_dir,
        imports_path
    ]
    
    print(f'running predictions for year: {year}')
    subprocess.run(predict_command, check=True)

# Use concurrent.futures to parallelize the year-by-year predictions
with concurrent.futures.ProcessPoolExecutor(max_workers=12) as executor:
    executor.map(run_prediction, years)

print("yeehaw, all predictions completed! :p ")






