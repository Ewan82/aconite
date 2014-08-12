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


