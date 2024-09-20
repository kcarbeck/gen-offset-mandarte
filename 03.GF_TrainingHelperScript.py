# use '03.gradient_training.R' script to fit GF model (based on Brandon Lind scripts)
# katherine carbeck
# 13 august 2024

##!

##! Bash
git clone https://github.com/brandonlind/pythonimports.git
##!



###*############################################*###
        ###*  1. run GF training script  *###
###*############################################*###
##! python
python
DIR = '/workdir/kcarbeck'
from pythonimports import *
import subprocess

#see imported modules:
import sys
for module_name in sys.modules:
     print(module_name)

train_script_file = f'{DIR}/03.gradient_training.R'  # Path to the training R script
snpfile = f'{DIR}/data/snpdir/snpfile_full.txt'
envfile = f'{DIR}/data/envdir/envfile.txt'
save_dir = f'{DIR}/data/training'
imports_path = f'{DIR}/r_imports'

# Run the training script once
train_command = [
    'Rscript',
    train_script_file,
    snpfile,
    envfile,
    save_dir,
    imports_path
]

print('Training the Gradient Forest model...')
subprocess.run(train_command, check=True)



"""
Warning message:
In randomForest.default(x = X, y = spec_vec, maxLevel = maxLevel,  :
  The response has five or fewer unique values.  Are you sure you want to do regression?
                  user                 system                elapsed
3741.72100000000000364  927.51299999999991996 4682.42100000000027649
Saving trained model ...
[1] "/workdir/kcarbeck/data/training/trained_gradient_forest_model.RDS"

DONE!
[1] "August 15 2024 18:17:30"
CompletedProcess(args=['Rscript', '/workdir/kcarbeck/03.gradient_training.R', '/workdir/kcarbeck/data/snpdir/snpfile_full.txt', '/workdir/kcarbeck/data/envdir/envfile.txt', '/workdir/kcarbeck/data/training', '/workdir/kcarbeck/r_imports'], returncode=0)
"""



