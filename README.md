# BL_data_generation
This github site contains tools to analyze the data for the paper entitled 
"Forward Premia in Electricity Markets: a replication study* by 
Silvester van Koten.

The site contains 
1. the data using grid search
2. the data using 
sampling
3. the stata do files to create all the stata (dta) files
4. the stata do files to analyze all the stata (dta) files


The site also contains the simulation programs, one in python and one in 
mathematica to generate the data. There is also a program written 
in Matlab that attempts to reproduce Figure 3 in
 Bessembinder & Lemmon (2002).


## Data
To use the data, first unzip (or unrar) the data files in the folders "data_in_stata_format", "data_in_raw_format_generated_in_python", "data_in_raw_format_generated_in_mathematica". Use an unrar tool for this. Choose "unrar here" (or "unzip here") 

Open the folder "Stata_analysis" > "Stata_analysis_home folder" > "stata_files"

- "2. Analyse data_main.do" can be used to replicate all the figures in the paper.
The data have been generated in Python.
Run the full dofile. Before you run the dofile, make sure to unrar all the
files in the folder "Stata_analysis_home folder\data_in_stata_format" 
with a rar-utility.

- "3. Analyse data_robust.do" replicates the figures using the data generated 
by brute sampling in Mathematica. The results should be identical to those
obtained with the dofile "2. Analyse data_main.do" 


In the folder is also a do-file to recreate the stata dta files from the raw csv data files:

- "1.Make data sets.do" can be used to generate the stata files 
from the raw data. Open the file, and set the stata working 
directory to "Stata_analysis_home folder". Then run the full code. 
Then you will create the stata dta files "exp_data_python.dta" and
"exp_data_mathematica.dta" in the folder "Stata_analysis_home folder\data_in_stata_format"


## Simulation programs

### Data_generation_by_grid_search_py36
The main simulation program is "Data_generation_by_grid_search_py36". 
To run the program, make sure the dependencies are installed. In particular, 
python 3.6, Numpy, Scipy, and Pandas.

Open Data_generation_by_grid_search_py36 > upper_loop.py and then run 
the program. Using pycharm, this can be done by opening the file upper_loop.py and
pressing ctrl-F10. The raw data are now generated in\
Data_generation_by_grid_search_py36>data.

There are thus in total 8 datapackages created. Each data package takes approximately 10~20 minutes to create (on a reasonably strong consumer type computer).

### Sampling_ Mathematica
Regarding footnote 12.
The simulation The main simulation program is "Data_generation_by_grid_search_py36".
To run the program, run Wolfram Mathematica 11. Open all the numbered files. 
Then run them one by one in the order indicated.

### Independent simulations Zelenay_ MATLAB
Regarding footnote 15.
Marek Zelenay wrote, independently and unwittingly of my analyses 
and results, on my request, an alternate simulation in Matlab 
to reproduce Figure 3 in Bessembinder & Lemmon (2002). 
His results are identical to mine.
