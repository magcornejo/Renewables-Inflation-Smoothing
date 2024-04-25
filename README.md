################################################################################
#		
# Title:	The transition to renewables: dampening the
#         impact of fossil fuel price shocks on local inflation
# 				
# Authors: 	Magdalena Cornejo, Universidad Torcuato Di Tella and CONICET 
# 		      Michelle Hallack, Florence School of Regulation
#           David Matias, Inter-American Development Bank
#		
################################################################################

################################################################################
# A. General instructions 
################################################################################

This project is designed to be executed in R Studio. 

Notes regarding reproducibility:

We suggest you run script files "svar.R" to get the structural shocks and 
"lp_panel.R" to obtain the local projections and reproduce the results in the paper. 

If you wish to contact to the authors redarging this project, please reach out to 
Prof. Magdalena Cornejo (mcornejo@utdt.edu).


################################################################################
# B. Description of data files
################################################################################

Folder path    Description

../data1      Raw datasets
../data2      Intermediate data files

File Name      Description

crude_oil.xlsx    Raw dataset for crude oil SVAR estimation
natural_gas.xlsx  Raw dataset for natural gas SVAR estimation
panel_data.xlsx    Raw dataset for panel local projections estimation
expenditure.xlsx  Raw dataset with government expenditure to GDP ratio
oil_shocks.csv    Intermediate data file with crude oil structural shocks
gas_shocks.csv    Intermediate data file with natural gas structural shocks


################################################################################
# C. Description of script files
################################################################################


- svar.R
Performs Structural VAR estimations for crude oil and natural gas markets
to identify the structural shocks on prices that will be used for the panel VAR.

Figures 1, A1 and A2 are obtained.

- lp_panel.R
Calculates and plots the local projections 

Figures 2, 3, 4, 5, A3, A4 and A5 are obtained.


MIT License

Copyright (c) 2024 Magdalena Cornejo

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.


The End
