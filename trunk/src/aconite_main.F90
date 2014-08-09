      program main
      use aconite_init
      use aconite_io
      use aconite_ecosystem
      use aconite_functions
      implicit none

      integer :: CN_Mode
      integer :: rstep
      integer :: mstep
      integer :: model_year
      character(len=4)in_spinup
      integer :: year_count
      integer :: cal_year
      
      print *, 'RUNNING ACONITE V1.1'
      print *, 'THOMAS AND WILLIAMS 2014 GEOSCIENTIFIC MODEL DEVELOPMENT'
      print *, '----------------------------'

      call initvars
      call read_namelist
      


      !read climate data
      call read_climate(clim,io%clim_length) 
      !read parameters, site, and disturbance
      if (io%param_in /= '') then
      call read_parameters()
      endif

      if (io%site_in /= '') then
      call read_site_data()
      endif
      
      if (io%dist_in /= '') then
      call read_dist_data()
      endif
      
      if (io%nfert_in /= '') then
      call read_nfert_data()
      endif
      
      if (io%soilwarm_in /= '') then
      call read_soilwarm_data()
      endif

      !read restart
      if (io%restart_in /= '') then
          call read_restart()
      endif
     
      model_year = 0
      cal_year = io%sim_start_year 
      
  
      print *,'STARTING SIMULATION'

      do mstep = 1,((io%sim_length+io%spinup_length)*365)
    
           state%rstep = state%rstep + 1
           if(state%rstep == (io%clim_length+1).OR. (mstep == (io%spinup_clim_length*365)+1)) then
               state%rstep = 1
           endif
           		
           if(mstep <= io%spinup_length*365) then
               in_spinup = 'YES'
               cal_year = io%sim_start_year 
           else
               in_spinup = 'NO'
           endif
           
           	year_count = year_count + 1
   
           call ecosystem_dynamics(state%rstep,year_count,cal_year)

           if (io%day_out /= '' .AND. mstep >= (io%print_year_start*365) .AND. mstep < (io%print_year_end*365)) then
              call write_daily_output(model_year,mstep,cal_year)
           endif
           
           if(year_count == 365 .AND. mstep >= ((io%spinup_length+io%print_year_start)*365) .AND. &
           	  mstep <= ((io%spinup_length+io%print_year_end)*365)) then   
              call write_annual_output(state%rstep,model_year,mstep,cal_year)
              if(io%restart_out /= '') then
              call write_restart()
              endif
           endif  
           
           call zero_fluxes()    

           if(year_count == 365) then
              print'(A21,1x,F4.0,1x,A15,1x,I4,1x,A14,1x,A4)', 'Number of sim. years',(mstep/365.0),&
              	'; calendar year',cal_year,'; in spinup? =',in_spinup
              cal_year = cal_year +1 
              year_count = 0 
           endif

      enddo

      end program main
