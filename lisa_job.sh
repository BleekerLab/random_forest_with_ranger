#!/bin/bash
#SBATCH -p normal 
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=m.galland@uva.nl
#SBATCH --cpus-per-task=16
#SBATCH --time=50:00:00
#SBATCH --nodes=1



# build the singularity image first 
singularity run randomforest.simg \
  --input_file data/housing.csv \
  --outdir results/ \
  --n_permutations 100 \
  --n_cores 16 \
  --variable_ratio 0.8 \
  --n_inner 7 \
  --n_outer 8 \
  --n_reps 50 
