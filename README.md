# P-budget
The files here include data sets and codes for the following manuscript: Characterizing Spatio-temporal Variability in Phosphorus Export Across the United States through Bayesian Hierarchical Modeling.

Items in this repository:

data.rds- This is a R data file that includes all the input data sets for the model. It needs to be imported into R with the appropriate command.

Stan_code.R- RStan code needed to run the models. The "rstan" (https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) and "rstudioapi" packages need to be installed prior to running the models. 
# Example
Step 1: Install "rstan" and "rstudioapi" packages. 

Step 2: Run the "stanmodelcode" annual model (line #4 in the "Stan_code.R" code) .

Step 3: Read the model input data set (data.rds), if necessary (line #66)

Step 4: Run the stan function (line #68). Suggest using the following parameters: iter=10000, warmup=5000, thin=5, chains=3,cores=3,adapt_delta =0.99 ,max_treedepth =25. The estimated runtime on desktop is about 2 hours. Note that more iterations and warmup steps may improve model convergence.
