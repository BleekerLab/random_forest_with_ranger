# Random Forest with ranger
An Rscript wrapper around the `ranger` package of R. Contains a Docker image to ensure portability of the R script.  

## Input file format
First column must contain the sample class. It must be a character not a number.      
All other columns must contain variable measurements. 

**Example:**
An example is given below. The first column is called "sample_class" and contains the sample class for breast cancer tumors. 
It has two levels "benign" or "malign". 

| sample_class  	| clump 	| uniformity_cell_size 	| uniformity_cell_shape 	| adhesion 	| epithelial_cell_size 	| bare_nuclei 	| chromatin 	| nucleoli 	| mitoses 	|
|--------	|-------	|----------------------	|-----------------------	|----------	|----------------------	|-------------	|-----------	|----------	|---------	|
| benign 	| 5     	| 1                    	| 1                     	| 1        	| 2                    	| 1           	| 3         	| 1        	| 1       	|
| benign 	| 5     	| 4                    	| 4                     	| 5        	| 7                    	| 10          	| 3         	| 2        	| 1       	|
| benign 	| 3     	| 1                    	| 1                     	| 1        	| 2                    	| 2           	| 3         	| 1        	| 1       	|
| benign 	| 6     	| 8                    	| 8                     	| 1        	| 3                    	| 4           	| 3         	| 7        	| 1       	|
| benign 	| 4     	| 1                    	| 1                     	| 3        	| 2                    	| 1           	| 3         	| 1        	| 1       	|
| malign 	| 8     	| 10                   	| 10                    	| 8        	| 7                    	| 10          	| 9         	| 7        	| 1       	|
| benign 	| 1     	| 1                    	| 1                     	| 1        	| 2                    	| 10          	| 3         	| 1        	| 1       	|
| benign 	| 2     	| 1                    	| 2                     	| 1        	| 2                    	| 1           	| 3         	| 1        	| 1       	|
| benign 	| 2     	| 1                    	| 1                     	| 1        	| 2                    	| 1           	| 1         	| 1        	| 5       	|

## Outputs

### Plots

### Table of candidates
