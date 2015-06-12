#How to use ACONITE

# Introduction #

A description of how to use ACONITE


# Details #

Requirements:
- Fortran compilers.

> We use GNU compilers for Apple OS X that can be found at http://hpc.sourceforge.net

Recommended

-  R

> We have provided R code that makes it easier to run ACONITE

To run

1) Compile the code by typing ./build\_aconite. If permission denied, type 'chmod u+x build\_aconite'

2) Modify the aconite\_namelist to customize the simulation.  The following files and inputs are required.  Be sure to include the full path of the files.
> CLIM\_FILE=  :  the daily climate inputs (use the '/input\_data' directory)

> PARAM\_FILE=  :  the parameter file (use the '/input\_data' directory)

> DAY\_OUT=   :  Enter a file name here if you want daily outputs (use the '/output' directory)

> ANNUAL\_OUT=  :  Enter a file name here if you want annual outputs.  Stocks will be output on the day of year specified by the 'ANNUAL\_STATE\_DOY' input below (use the '/output' directory)

> RESTART\_IN=  :Enter the initial conditions file.  If blank it will use the hard coded defaults which aren't guaranteed to work (use the '/input\_data' directory)

> RESTART\_OUT=  : Enter a file name if you want the model save an initial condition file for future use (use the '/input\_data' directory)

> DIST\_IN =  : OPTIONAL: file describing disturbance history (use the '/input\_data' directory)

> SITE\_IN=    : The site level information (use the '/input\_data' directory)

> NFERT\_IN	: OPTIONAL: file describing the N fertilization experiment (use the '/input\_data' directory)

> SOILWARM\_IN	: OPTIONAL: file describing the soil warming experiment (use the '/input\_data' directory)

> SIM\_LENGTH=2000 : This is the total number of years in the simulation

> CLIM\_LENGTH=365 : This is the total number of days in the climate input file

> DIST\_LENGTH : number of years in the distaff\_in file (doesn't include the header)

> NFERT\_LENGTH	 : number of years in the nfert\_in file (doesn't include the header)

> SOILWARM\_LENGTH	: number of years in the soilwarm\_in file (doesn't include the header)

> YEAR\_OUT\_START : the year that output is first saved

> YEAR\_OUT\_END : the year that output is last saved

> ANNUAL\_STATE\_DOY=200 : the day of year that stocks are saved in the annual output

> COLD\_START=0			     : overwrites the initial stock variables so that C:N are consistent with the parameterized C:N ratios. Rarely used.

> SPINUP\_LENGTH=			:  the number of year

> SPINUP\_CLIM\_LENGTH=                 : the number of days of climate data used in the model spin-up

3) Run the compiled code.  ('./run\_aconite')

Structure of ACONITE directories

build\_aconite:  a bash script to compile code

/src  :  contains the source code and makefile for compiling
_type.f90	declare variables_

> aconite\_init.f90  : variables allocated, hard wire parameters, zero fluxes, initialize states

> aconite\_io.f90	: reads namelist, sets climate, parameter initialization if in namelist, writes output files

> aconite\_main.f90  :	loop through time, call initialization functions, calls read climate

> aconite\_ecosystem.f90 : processes

> aconite\_function.f90	  : repeated functions (photosynthesis, N uptake, etc), error checks, annual updates

> makefile :  used to compile the code

/docs : includes files that help better understand ACONITE
/input\_data : parameter files, climate files, disturbance files, site descriptions, etc
/scripts  : R code for running simulations and plotting the results

> To use the scripts:
  1. Open R
> 2) Set working directory to be the scripts directory <setwd('directory')>
> 3) Type 'Source(run\_aconite\_decid.r') - or _evergreen or_tropical
> 4) Type

> run\_aconite(file1,file2,plot\_day,total\_sim,start,end,annual\_doy,spinup\_length)
> > -file1 = 'daily file'
> > -file2=	'annual file'
> > > - plot day = 0 (no) or 1(yes) for daily output
> > > -annual\_doy = day of year of the stocks

> > - spinup\_length = number of years that the model is run before starting the total\_sims


> example:
> > run\_aconite('','annual out.txt',0,2000,1,2000,200,0)
> > run\_aconite('dayout.txt','annual out.txt',1,2000,1999,2000,200,0)


Have fun!