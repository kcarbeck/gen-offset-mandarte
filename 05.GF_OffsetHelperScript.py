# use '05.gradient_offset_script.R' to calculate genomic offset
# katherine carbeck
# 13 august 2024

###*################################################*###
###*  3. calculate offset between consecutive years *###
###*################################################*###
# on terminal so enter python env:
python
from pythonimports import *
import xarray, rioxarray
import subprocess
DIR = '/workdir/kcarbeck'


#see imported modules:
import sys
for module_name in sys.modules:
     print(module_name)
     

years = range(1971, 2023)
offset_script_file = f'{DIR}/05.gradient_offset_script.R'  # Path to the R script for calculating offset
save_dir = f'{DIR}/data/offsets'  # Directory where the offset results will be saved
#model_dir = f'{DIR}/data/training'  # Directory where the trained model is stored
range_file = f'{DIR}/data/rasterdir/RasterStackTxtFiles/raster_data_1971.txt'
imports_path = f'{DIR}/r_imports'  # Path to your custom R functions

for year in years:
    pred_file_year1 = f'{DIR}/data/predictions/out_{year}_gradient_forest_predOut.RDS'
    pred_file_year2 = f'{DIR}/data/predictions/out_{year + 1}_gradient_forest_predOut.RDS'
    offset_basename = f'offset_{year}_{year + 1}'
    
    offset_command = [
        'Rscript',
        offset_script_file,
        pred_file_year1,
        pred_file_year2,
        range_file,
        offset_basename,
        save_dir,
        imports_path
    ]
    
    print(f'Calculating offset between {year} and {year + 1}')
    subprocess.run(offset_command, check=True)

print("congrats cowboy, all offsets calculated & saved!")


