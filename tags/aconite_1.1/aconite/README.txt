How to ACONITE

1) Compile the code by typing ./build_aconite. If permission denied, type 'chmod u+x build_aconite'

2) Modify the aconite_namelist to customize the simulation.  The following files and inputs are required.  Be sure to include the full path of the files.  
	CLIM_FILE=  :  the daily climate inputs (use the '/input_data' directory)
        PARAM_FILE=  :  the parameter file (use the '/input_data' directory)
        DAY_OUT=   :  Enter a file name here if you want daily outputs (use the '/output' directory)
	ANNUAL_OUT=  :  Enter a file name here if you want annual outputs.  Stocks will be output on the day of year specified by the 'ANNUAL_STATE_DOY' input below (use the '/output' directory)
	RESTART_IN=  :Enter the initial conditions file.  If blank it will use the hard coded defaults which aren't guaranteed to work (use the '/input_data' directory)
	RESTART_OUT=  : Enter a file name if you want the model save an initial condition file for future use (use the '/input_data' directory)
	DIST_IN =  : OPTIONAL: file describing disturbance history (use the '/input_data' directory)	
	SITE_IN=    : The site level information (use the '/input_data' directory)
	NFERT_IN	: OPTIONAL: file describing the N fertilization experiment (use the '/input_data' directory)
	SOILWARM_IN	: OPTIONAL: file describing the soil warming experiment (use the '/input_data' directory)
	SIM_LENGTH=2000			     : This is the total number of years in the simulation
	
        CLIM_LENGTH=365			     : This is the total number of days in the climate input file
	DIST_LENGTH		: number of years in the distaff_in file (doesn't include the header)
	NFERT_LENGTH		: number of years in the nfert_in file (doesn't include the header)
	SOILWARM_LENGTH		: number of years in the soilwarm_in file (doesn't include the header)
	YEAR_OUT_START			 : the year that output is first saved
        YEAR_OUT_END                     : the year that output is last saved
	ANNUAL_STATE_DOY=200		     : the day of year that stocks are saved in the annual output
	COLD_START=0			     : overwrites the initial stock variables so that C:N are consistent with the parameterized C:N ratios. Rarely used.
	SPINUP_LENGTH=			:  the number of year  	
	SPINUP_CLIM_LENGTH=                 : the number of days of climate data used in the model spin-up

3) Run the compiled code.  ('./run_aconite')


ACONITE file structure

_type.f90	declare variables

_init.f90	variables allocated, hard wire parameters, zero fluxes, initialize 			states

_io.f90	reads namelist, sets climate, parameter initialization if in namelist
		writes output files

_main.f90	loop through time, call initialization functions, calls read climate

_ecosystem.f90	processes

_function.f90	repeated functions and annoying things, error checks, annual 			updates

_namelist	where you change climate file, input/output file names

run_decidious_aconite.r
	source('run_decidious_aconite.r')
	run_aconite <- function(file1,file2,plot_day,total_sim,start,end,annual_doy,spinup_length)
		-file1 = 'daily file'
		-file2=	'annual file'
                       - plot day = 0 (no) or 1(yes)
                       -annual_doy = day of year of the stock
		- spinup_length = number of years that the model is run before starting the total_sims

            example:
                     run_aconite('','annual out.txt',0,2000,1,2000,200,0)
                     run_aconite('dayout.txt','annual out.txt',1,2000,1999,2000,200,0)
makefile 	type 'make -f makefile_mac'

Using sftp
       put goes Dropbox to cluster
       get goes from cluster to dropbox

Manaus Files
   Parameter file: parameter_tropical.txt
   Initialization file: tropical_restart.txt
   Site file:  site_details_Manaus.txt

Harvard Forest (Deciduous) files 
    Parameter file: parameter_HB_decid.txt
   Initialization file: decid_restart.txt
   Site file:  site_details_HF.txt

Harvard Forest (evergreen) files 
    Parameter file: parameter_HB_evergreen.txt
   Initialization file: seasonal_evergreen_restart.txt
   Site file:  site_details_HF.txt

Hubbard Brook (Deciduous) files 
    Parameter file: parameter_HB_decid.txt
   Initialization file: decid_restart.txt
   Site file:  site_details_HBEF.txt

Hubbard Brook (evergreen) files 
    Parameter file: parameter_HB_evergreen.txt
   Initialization file: seasonal_evergreen_restart.txt
   Site file:  site_details_HBEF.txt

Current list met files:
HF_2002.csv
HF_2000_2012_noleap.csv
HBEF_1960_2004_noleap.csv
Manaus_1999.csv






