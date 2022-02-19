# California Spatial Modeling Project

This GitHub repository houses the University of California, Davis Energy Futures Program's spatial model of the transportation and electricity grid system in California to investigate scenarios of a hydrogen economy.  The project is divided into a transport demand and infrastructure module and an electricity grid module.

## Travel Demand Module

## Electricity Grid Module
The electricity grid module is a sub-version of the Grid Operation Optimized Dispatch (GOOD) model.  The model is a detailed economic dispatch model with simplified capacity expansion capabilities.  The base version of the GOOD model simulates the operation of power plants across a regional specification given an input electricity demand load.  In the California Spatial Modeling Project, the GOOD model is modified to operate in the Western Interconnect and contains specific modifications to enable hydrogen production and storage.  Below is a model diagram that provides a basic schematic of data flow into the model, a breakdown of the major parsing components of the model and the GOOD solver, and the outputs resulting from model runs.  The GOOD platform is run with two software systems: [R](https://www.r-project.org/) and [GAMS](https://www.gams.com/).  The current instation of the model uses R v4.0.2 and GAMS v25.1.3 (with the CPLEX solver), though any subsequent versions should be compatible.

![Screenshot](./grid_module/good_diagram.png)

*base.r* - This is the base file for the GOOD model which loads all of the libraries and helper functions used in subsequent .r files.
*good.gms* - This file is the optimization routine run by the General Algebraic Modeling Software (GAMS)
*good.r*
*prep-inputs_grid.r*
*prep-inputs-mobility.r*

*parse-outputs.r*


*inputs*

(for access to data files, please contact: ajenn@ucdavis.edu)
*runFiles*
