# testsim Documentation
Testsim makes simulating data very easy and fast. It also comes with builtin statistical tests and a function for monte-carlo simulations.
## Installation
Install the package via the git repository (requiring the devtools package):
````R
devtools::install_github("Carsten134/testsim")
````
Or clone the repository into your desired working dir:
````
cd C:\your\work\dir

git clone https://github.com/Carsten134/testsim
````
Then launch a R-session in the created dir testsim and run:
````R
devtools::install()
````
## Usage in a nutshell
To generate samples, first generate densities. You can choose between normal, uniform and cauchy destributions:
````R
params <- data.frame(mean = 1:10, var=rep(1,10))

dens <- gen_rnorm_vector(params)
````

You can easily generate samples with any density funcitons
````R
data <- gen_ksample(dens)
````
