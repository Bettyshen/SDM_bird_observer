#!/bin/bash
#SBATCH --job-name=TrainBRT-Rjob          # Job name
#SBATCH --output=BRTres.txt           # Output file
#SBATCH --error=BRTerror.txt          # Error file
#SBATCH --time=72:00:00            # Time limit hh:mm:ss
#SBATCH --partition=share          # Partition
#SBATCH --ntasks=4                 # Run on CPUs
#SBATCH -c 1                                         # number of cores/threads per task (default 1)
#SBATCH --mem=240G                   # Job memory request


# Load the R module (if required)
module load R/4.3.3
module load gdal/3.0.4
module load geos/3.7.2
module load proj/8.2.1
module load sqlite
# Record start time
date
Rscript /nfs/stak/users/shenf/hpc-share/dynamicSDM/BRT_train/Betty_Step2_BRT_Training.R  # Path to R script
# Record end time
date

exit
