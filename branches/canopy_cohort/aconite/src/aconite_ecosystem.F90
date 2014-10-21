      module aconite_ecosystem

      public :: ecosystem_dynamics 
 
!EOP
!-----------------------------------------------------------------------

     contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
      subroutine ecosystem_dynamics(rstep,year_count,cal_year)

     use aconite_type 
     use aconite_functions
     implicit none 

! !OTHER LOCAL VARIABLES:
	  integer :: i
      real :: r,z, z2, decl, LatRad,h,TA,AC,hr,es,delta,emean, GDD
      integer :: rstep
      real :: total_immob
      real :: litter_to_atm
      real :: litter_to_soil
      integer :: year_count
      integer :: cal_year
      integer :: pass
      real :: avail_nh4
      real :: avail_no3
      real :: growth_potential
      real :: avail_C,avail_N
      real :: Ra_temp_resp,Rh_temp_resp
      real :: potN,labileC_bud2labile_Ra,labileN_bud2labileN
      real :: tmp_a_woodN,tmp_a_woodC,tmp_leafC,tmp_leafN,instant_Creturn_leafCN
      real, parameter :: pi = 3.141592653589793239
      real :: litterfallC,litterfallN,rt, gross_nmin, Nav, avail_nh4_conc,avail_no3_conc
      real :: krate_adjust,q_adjust
      real :: leafC_dist,leafN_dist
!EOP
!-----------------------------------------------------------------------


!---CALCULATE ATMOSPHERIC ENVIRONMENT----------------------------------

      state%Tave =  (clim%tmin(rstep) + clim%tmax(rstep)) / 2.0
      state%Tday = (clim%tmax(rstep) + state%Tave) / 2.0;
      LatRad = site%Lat * (2.0 * pi) / 360.0;
      r = 1 - (0.0167 * cos(0.0172 * (clim%doy(rstep) - 3)));
      z = 0.39785 * sin(4.868961 + 0.017203 * clim%doy(rstep) + & 
         0.033446 *sin(6.224111 + 0.017202 * clim%doy(rstep)));
      if (abs(z) < 0.7) then
          decl = atan(z / (sqrt(1.0 - (z**2))));
      else
          decl = pi / 2.0 - atan(sqrt(1 - z**2) / z);
      endif
      if (abs(LatRad) >= (pi/2.0)) then
          if (site%Lat < 0) then
              LatRad = (-1.0) * (pi/2.0 - 0.01);
          else
              LatRad = (1.0) * (pi/2.0 - 0.01);
          endif
      endif
      z2 = -tan(decl) * tan(LatRad);

      if (z2 >= 1.0) then
         h = 0;
      elseif (z2 <= -1.0) then
         h = pi;
      else
         TA = abs(z2);
         if (TA < 0.7) then
             AC = 1.570796 - atan(TA / sqrt(1.0 - (TA**2)));
         else
             AC = atan(sqrt(1 - (TA**2)) / TA);
         endif
         if (z2 < 0) then
             h = pi-AC;
         else
             h = AC;
        endif
      endif
      hr = 2.0 * (h*24.0) / (2.0*pi);
      state%DayLength = (hr);
      state%NightLength = (24.0 - hr);
     
      GDD = state%Tave - 8
      if (GDD < 0) then
          GDD = 0
      endif
      state%GDDTot = state%GDDTot + GDD

!---------------------------------------------------------------------
! CALCULATE ECOSYSTEM FLUXES

       if(year_count == 1) then
          call year_update(1,year_count)     
       endif


!--- PLANT STRUCTURE ------------------------ 

      state%lai = state%leafC / param%lca
    
!----- RESPIRATION TEMPERATURE RESPONSE ---------
      if(year_count == 1) then	
      state%soilwarm_degree = 0.0
        do i = 1,io%soilwarm_length
             
        	if(site%soilwarmyear(i)  == cal_year) then
        		state%soilwarm_degree = site%soilwarm_degree(i)
        		        	
        	endif
        enddo
      endif
      
      Ra_temp_resp = param%Ra_Q10**(((state%Tave)-20)/10)
      Rh_temp_resp = param%Rh_Q10**(((state%Tave+state%soilwarm_degree)-20)/10) 
      
!-----TISSUE TURNOVER ------------------------

       if(clim%doy(rstep) >= param%SenescStart) then	!after sensence date, drop leaves from current cohort
       		flux%leafC_c1_to_c2 = state%leafC_c1
       		flux%leafN_c1_to_c2 = state%leafC_c2
       endif 
       		
      flux%t_leafC = state%leafC_c2*param%t_leaf
      flux%t_leafN = state%leafN_c2*param%t_leaf * (1 - param%trans_frac)
      flux%retransN = state%leafN_c2*param%t_leaf * (param%trans_frac)
      
      flux%t_woodC = state%woodC * param%t_wood
      flux%t_rootC = state%rootC * param%t_root
      flux%t_woodN = state%woodN * param%t_wood
      flux%t_rootN = state%rootN * param%t_root


!--- LITTER AND SOIL CALCULATIONS --------------------------------

     !SOIL MODEL IN GERBER ET AL. 2010 GLOBAL BGC CYCLES
	  avail_nh4 = state%nh4-(param%bufferNH4*state%nh4)/(1+param%bufferNH4)
      avail_no3 = state%no3-(param%bufferNO3*state%no3)/(1+param%bufferNO3)
      	avail_nh4_conc = max(avail_nh4/param%rooting_depth,0.0)
      	avail_no3_conc = max(avail_no3/param%rooting_depth,0.0)
      	Nav = avail_nh4_conc + avail_no3_conc
      
      	krate_adjust = max(0.,(1-param%xi*Nav))
      	q_adjust = (Nav/(param%k_S+Nav))

      	litterfallC = flux%t_leafC+ flux%t_woodC + flux%t_rootC 
      	litterfallN = flux%t_leafN+ flux%t_woodN + flux%t_rootN
 
      	rt = litterfallC/litterfallN
      	
      	flux%litterC_to_soil1= litterfallC * max((param%aLF - param%bLF*param%flig*rt),param%flig_min)
      	flux%litterC_to_soil2= litterfallC * (1-max((param%aLF - param%bLF*param%flig*rt),param%flig_min))
      	flux%litterN_to_soil1= litterfallN*(max((param%aLF - param%bLF*param%flig*rt),param%flig_min) &
      		*min((rt/param%rls_min),1.))
      	flux%litterN_to_soil2= litterfallN*(1-(max((param%aLF - param%bLF* param%flig*rt),param%flig_min)&
      		*min((rt/param%rls_min),1.)))
      	
	  	flux%soil1_to_atm_C = state%soilC_1*Rh_temp_resp*param%kLF

      	flux%soil2_to_soil3_C = Rh_temp_resp*param%kLS*krate_adjust*state%soilC_2*param%qmax*q_adjust
      	flux%soil2_to_soil4_C= Rh_temp_resp*param%kLS*krate_adjust*state%soilC_2*param%q_SP
      	flux%leachDOC = (Rh_temp_resp*param%kLS*krate_adjust*state%soilC_2)*param%f_DOM
      	flux%leachDON = flux%leachDOC/param%r_DOM
      	flux%soil2_to_atm_C = (Rh_temp_resp*param%kLS*krate_adjust*state%soilC_2) - &
      		flux%soil2_to_soil3_C - flux%soil2_to_soil4_C 
      	flux%soil3_to_atm_C = state%soilC_3*Rh_temp_resp*param%kSS
      	flux%soil4_to_atm_C = state%soilC_4*Rh_temp_resp*param%kSP
      	
      	flux%soil2_to_soil3_N = flux%soil2_to_soil3_C/param%r_SS
      	flux%soil2_to_soil4_N = flux%soil2_to_soil4_C/param%r_SP
 
      	flux%soil1_to_NH4 = state%soilN_1*Rh_temp_resp*param%kLF     
      	flux%soil2_to_NH4 = Rh_temp_resp*param%kLS*krate_adjust*state%soilN_2
      	flux%soil3_to_NH4 = state%soilN_3*Rh_temp_resp*param%kSS
      	flux%soil4_to_NH4 = state%soilN_4*Rh_temp_resp*param%kSP 
      	
	  	gross_nmin = flux%soil1_to_NH4 + flux%soil2_to_NH4 + flux%soil3_to_NH4 + flux%soil4_to_NH4
	  	
	  	!NOTE:TO PREVENT NEGATIVE NH3 OR NO3
	  	if(Nav > 0) then
      		flux%nh4_immob = Rh_temp_resp*param%kLS*krate_adjust*(avail_nh4_conc/Nav)*((state%soilC_2/param%r_SS) &
      			*param%qmax*q_adjust+ param%q_SP/param%r_SP)
      		flux%no3_immob = Rh_temp_resp*param%kLS*krate_adjust*(avail_no3_conc/Nav)*((state%soilC_2/param%r_SS) &
      			*param%qmax*q_adjust+ param%q_SP/param%r_SP) 
      	else
		 	flux%nh4_immob = 0.0
		 	flux%no3_immob = 0.0
		 	!NOTE: ALL DECOMPOSITION GOES TO THE ATMOSPHERE
		 	flux%soil2_to_atm_C = 	flux%soil2_to_atm_C + flux%soil2_to_soil3_C
		 	flux%soil2_to_soil3_C = 0.0
      	endif

        flux%Rh_total = flux%soil1_to_atm_C + flux%soil2_to_atm_C + flux%soil3_to_atm_C + &
        	flux%soil4_to_atm_C 

!------ PHOTOSYNTHESIS (GPP) ------
        
      if(state%lai > 0) then
          flux%GPP = Photosyn((state%leafN), state%lai, state%rstep)
      else
          flux%GPP = 0.0
      endif 
       
!----  PLANT N UPTAKE ----------

	  !NOTE: NO BUFFERING OF AVAIL N B/C BUFFERING OCCURS IN UPTAKE FUNCTION
	  avail_nh4 = state%nh4 - flux%nh4_immob  
	  avail_no3 = state%no3 - flux%no3_immob
	  
      if(state%labileN > state%maxNstore) then
	   		state%Nuptake_downreg = 0.0 
	  else
	        state%Nuptake_downreg = 1.0 !-(state%labileN/state%maxNstore)
	  endif

      flux%nh4_uptake = Nupt(1, state%rootC, state%rootN, state%woodC,avail_nh4,state%Nuptake_downreg,flux%GPP,Ra_temp_resp)
		
      if(avail_nh4 >= flux%nh4_uptake) then
         avail_nh4 = avail_nh4 - flux%nh4_uptake
      else
         flux%nh4_uptake = max(avail_nh4,0.0)
         avail_nh4 = 0.0
      endif
      
      flux%no3_uptake = Nupt(2, state%rootC, state%rootN, state%woodC,avail_no3,state%Nuptake_downreg,flux%GPP,Ra_temp_resp)

      if(avail_no3 >= flux%no3_uptake) then
         avail_no3 = avail_no3 - flux%no3_uptake
      else
         flux%no3_uptake = max(avail_no3,0.0)
         avail_no3 = 0.0
      endif

!---- ALLOCATION ------------------------------

      CALL marginals(avail_NH4,avail_NO3,Ra_temp_resp)
      CALL marginal_integrator 
      
      state%target_rootCN = param%rootCN  ! SET HERE FOR NOW UNTIL WE GET DYNAMIC ROOT CN
      avail_C = max(state%labileC,0.0)
      avail_N = max(state%labileN,0.0)

      ! STEP 1: ALLOCATE ALL BUDC AND BUDN IN FIRST DAY OF SPRING LEAF OUT 
			
      instant_Creturn_leafCN = (marg%GPP_leafCN - marg%Rm_leafCN)* &
             (param%SenescStart - clim%doy(state%rstep)+1/param%t_leaf) -  marg%Rg_leafC - param%add_C

      if((state%GDDTot >= param%GDDStart) .AND. (clim%doy(rstep) < param%SenescStart)) then   
        growth_potential = param%growth_potential_param*min(1.,(state%labileN/state%maxNstore))
      else
        growth_potential = 0.0	
      endif
           
	  if(growth_potential > 0.0) then
	  	    
	  	tmp_leafC = state%leafC
	  
	 	if((instant_Creturn_leafCN >= 0 .and. state%leafC < state%maxleafC) .or. state%leafC_c1 == 0.0) then
		  if(state%leaf_out_doy == 0) then
		  	state%leaf_out_doy = clim%doy(state%rstep)
		  endif
	      flux%a_labileC_bud_2leaf = min(state%labileC_bud, (state%maxleafC-state%leafC))
          flux%a_labileN_bud_2leaf = min(state%labileN_bud,  &
          	 ((state%labileN_bud/state%labileC_bud)*flux%a_labileC_bud_2leaf))
          state%min_wood_deficit = state%min_wood_deficit+ &
          	  flux%a_labileC_bud_2leaf*param%Minleaf2woodAlloc
          state%wood_requirement = state%wood_requirement + flux%a_labileC_bud_2leaf*param%Minleaf2woodAlloc
          
          tmp_leafC = state%leafC + flux%a_labileC_bud_2leaf
          tmp_leafN = state%leafN + flux%a_labileN_bud_2leaf
      	 
    	endif
	  
     	if(avail_N < 0.0) avail_N = 0.0
        if(avail_C < 0.0) avail_C = 0.0
     
     	if(tmp_leafC < state%maxleafC .AND. state%labileC_bud < state%maxleafC*param%leafC2budCprop .AND.  &
     	instant_Creturn_leafCN > 0) THEN
! Continue filling the buds

         	flux%a_labileC_bud = min((avail_C * growth_potential)*(1-param%Ra_grow),(state%maxleafC - tmp_leafC))
         	flux%a_labileC_bud = max(0.0,flux%a_labileC_bud)
         	!CLOSE LEAFC IS CLOSE TO MAXLEAFC ADD LEAFC TO REACH MAXLEAFC
         	if((tmp_leafC+flux%a_labileC_bud) > state%maxleafC*param%stocks_close_prop .AND. &
         	tmp_leafC < state%maxleafC .AND. avail_C > (state%maxleafC-tmp_leafC)) then
            	flux%a_labileC_bud =  (state%maxleafC - tmp_leafC)
         	endif
                  
         	! CHECK THAT THERE IS ENOUGH N
         	potN = flux%a_labileC_bud / state%target_leafCN
         	if(potN > avail_N*growth_potential) then
         	!if(potN > avail_N) then
            	flux%a_labileC_bud = avail_N*growth_potential * state%target_leafCN
         	endif
         	flux%Ra_grow = flux%a_labileC_bud*(param%Ra_grow)
         	flux%a_labileN_bud = flux%a_labileC_bud / state%target_leafCN
         	state%leafN_deficit = state%leafN_deficit + potN - flux%a_labileN_bud
         	state%total_N_deficit = state%total_N_deficit + potN - flux%a_labileN_bud

         	avail_C = avail_C - flux%a_labileC_bud - flux%Ra_grow
         	avail_N = avail_N - flux%a_labileN_bud
         	
          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0
          
          if(state%labileC_Ra < state%maxRaC) then
            	flux%a_labileC_2Ra = min((state%maxRaC-state%labileC_Ra),(avail_C))
            	avail_C = avail_C - flux%a_labileC_2Ra
          endif

!IF MAXLEAFC IS MET OR LEAF ALLOCATION DOESN'T HAVE A POSITIVE RETURN        
     	else
! START REFILLING THE RESPIRATION POOL

          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0
          
          if(state%labileC_Ra < state%maxRaC) then
            	flux%a_labileC_2Ra = min((state%maxRaC-state%labileC_Ra),(avail_C))
            	avail_C = avail_C - flux%a_labileC_2Ra
          endif
          
          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0

! START REFILLING BUDS  
   
        	if(state%labileC_bud < state%maxleafC*param%leafC2budCprop) then
            	if((avail_C * growth_potential)*(1-param%Ra_grow) < (state%maxleafC* &
            	param%leafC2budCprop - state%labileC_bud)) then
          			flux%a_labileC_bud = (avail_C * growth_potential)*(1-param%Ra_grow)
        					
            	else
          			flux%a_labileC_bud = (state%maxleafC*param%leafC2budCprop - state%labileC_bud)
          		endif

    			potN = flux%a_labileC_bud / state%target_leafCN
         	if(potN > avail_N*growth_potential) then
         	!if(potN > avail_N) then
          			flux%a_labileC_bud = avail_N*growth_potential * state%target_leafCN
          		endif
          		flux%Ra_grow = flux%a_labileC_bud*(param%Ra_grow)
          		flux%a_labileN_bud = flux%a_labileC_bud / state%target_leafCN
          		avail_C = avail_C - flux%a_labileC_bud - flux%Ra_grow
          		state%leafN_deficit = state%leafN_deficit + potN - flux%a_labileN_bud   
          		state%total_N_deficit = state%total_N_deficit + potN - flux%a_labileN_bud         		
            	avail_N = avail_N - flux%a_labileN_bud

        	endif
        	
! START GROWING NEW WOOD
 
          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0
          	
          if(state%min_wood_deficit > 0.0) then
            	flux%a_woodC = min(avail_C*growth_potential*(1-param%Ra_grow),state%min_wood_deficit)
            	potN = flux%a_woodC / param%woodCN
  
         	if(potN > avail_N*growth_potential) then
         	!if(potN > avail_N) then
         			flux%a_woodC = avail_N*growth_potential * param%woodCN
            	endif
            	flux%a_woodN = flux%a_woodC / param%woodCN
            	avail_C = avail_C - flux%a_woodC - flux%a_woodC*param%Ra_grow
            	flux%Ra_grow = flux%Ra_grow+flux%a_woodC*param%Ra_grow
            	state%min_wood_deficit = state%min_wood_deficit - flux%a_woodC
            	state%total_N_deficit = state%total_N_deficit + potN - flux%a_woodN
            	avail_N = avail_N - flux%a_woodN  
          endif
     	
          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0

!START TRYING TO REACH MAX ROOT C      
        	if(state%rootC < state%maxrootC) then
            	flux%a_rootC = min((avail_C*growth_potential*(1-param%Ra_grow)), &
              	(state%maxrootC-state%rootC))
            	potN = flux%a_rootC / state%target_rootCN
         	if(potN > avail_N*growth_potential) then
         	!if(potN > avail_N) then
            	    flux%a_rootC = avail_N*growth_potential * param%rootCN
          		endif
            	flux%a_rootN = flux%a_rootC / state%target_rootCN
            	avail_C = avail_C - flux%a_rootC - flux%a_rootC*param%Ra_grow
            	flux%Ra_grow = flux%Ra_grow+flux%a_rootC*param%Ra_grow
            	state%total_N_deficit = state%total_N_deficit + potN - flux%a_rootN
            	avail_N = avail_N - flux%a_rootN  
         	endif
         	

          if(avail_N < 0.0) avail_N = 0.0
          if(avail_C < 0.0) avail_C = 0.0
			
        	if(state%leafC +flux%a_leafC>= state%maxleafC*param%stocks_close_prop .and. &
      		state%rootC +flux%a_rootC>= state%maxrootC*param%stocks_close_prop .AND. &
        	state%labileC_Ra+flux%a_labileC_2Ra >= state%maxRaC*param%stocks_close_prop .AND. &
        	state%min_wood_deficit == 0 .AND. avail_C > state%maxCstore) then
            	tmp_a_woodC = (avail_C-state%maxCstore)*growth_potential*(1-param%Ra_grow)
            	potN = tmp_a_woodC / param%woodCN
         	if(potN > avail_N*growth_potential) then
         	!if(potN > avail_N) then
          			tmp_a_woodC = avail_N*growth_potential * param%woodCN     			
          		endif
            	tmp_a_woodN = tmp_a_woodC / param%woodCN
            	avail_C = avail_C - tmp_a_woodC - tmp_a_woodC*param%Ra_grow
            	flux%Ra_grow = flux%Ra_grow+tmp_a_woodC*param%Ra_grow
            	flux%a_woodC = flux%a_woodC + tmp_a_woodC
            	flux%a_woodN = flux%a_woodN + tmp_a_woodN                 
            	avail_N = avail_N - tmp_a_woodN 
             	avail_C = avail_C - tmp_a_woodC

        	endif
        	
        	if(avail_N < 0.0) avail_N = 0.0
            if(avail_C < 0.0) avail_C = 0.0
      	
        	if(avail_C > state%maxCstore .and. avail_N < state%maxNstore) then
        			flux%Ra_excessC =  (avail_C - state%maxCstore)*growth_potential*param%t_excessC
      	    		flux%nfix = 0.0 !flux%Ra_excessC*param%Nfix_per_gC*Ra_temp_resp*state%Nuptake_downreg
      	  		    avail_C = avail_C - flux%Ra_excessC
      	  	endif        	

     	endif
     
     endif
     
!FILL RESPIRATION IF IT DIPS BELOW MAX 
     if(growth_potential == 0.0 .AND. state%labileC_Ra < state%maxRaC .AND. avail_C > 0) then   
        flux%a_labileC_2Ra = min((state%maxRaC-state%labileC_Ra),(avail_C))
        avail_C = avail_C - flux%a_labileC_2Ra
     endif 
    
!----  NITRIFICATION -------------

	  avail_nh4 = avail_nh4-(param%bufferNH4*avail_nh4)/(1+param%bufferNH4)
      flux%nitr = avail_nh4 * param%nitr_rate * Rh_temp_resp 
      if(avail_nh4 >=flux%nitr) then
         avail_nh4 = avail_nh4 - flux%nitr 
      else
         flux%nitr = max(avail_nh4,0.0)
         avail_nh4 = 0.0
      endif
             
!------- LEACHING -----------------

      avail_no3 = avail_no3-(param%bufferNO3*avail_no3)/(1+param%bufferNO3)
      flux%leachN = avail_no3 * param%leach_rate
      if(avail_no3 >= flux%leachN) then
        avail_no3 = avail_no3 - flux%leachN
      else
        flux%leachN = max(avail_no3,0.0)
        avail_no3 = 0.0
      endif

!------ N INPUTS --------------------

      flux%ndep_nh4 = clim%NH4dep(rstep)
      flux%ndep_no3 = clim%NO3dep(rstep)
      
      if(year_count == 1) then
        flux%nfert_nh4 = 0.0
        flux%nfert_no3 = 0.0
        do i = 1,io%nfert_length
        	if(site%nfertyear(i)  == cal_year) then
        		flux%nfert_nh4 = site%nfert_nh4(i)/365.
        		flux%nfert_no3 = site%nfert_no3(i)/365.
        	endif
        enddo
     endif
      
!----- AUTOTROPHIC RESPIRATION --------------

      if(param%use_reich == 0) then
          flux%Ra_Main = (state%leafN + state%rootN)*param%Ra_per_N*Ra_temp_resp
      else
          flux%Ra_Main = reich_resp(state%leafC,state%leafN,param%leaf_resp_A,&
             param%leaf_resp_B)*Ra_temp_resp + reich_resp(state%rootC,state%rootN,&
             param%leaf_resp_A,param%leaf_resp_B)*Ra_temp_resp
      endif
      
      flux%Ra_retrans = 0.0 !NOT CURRENTLY USED

!--- MOVEMENT BETWEEN STORAGE POOLS IF LABILE C GOES NEGATIVE------
      ! THIS PREVENTS THE RESPIRATION POOL FROM GOING NEGATIVE BY USING SOME OF THE BUD C
      ! IT "PUNISHES" THE VEGETATION FOR AN ABNORMALLY LONG WINTER

      if((state%labileC_Ra + flux%a_labileC_2Ra - flux%Ra_main) < 0) then

           flux%a_labileC_2Ra = flux%a_labileC_2Ra + min(flux%Ra_main,state%labileC)          
           if(state%labileC_bud > -(state%labileC_Ra + flux%a_labileC_2Ra - flux%Ra_main)) then
           		labileC_bud2labile_Ra = -(state%labileC_Ra + flux%a_labileC_2Ra - flux%Ra_main)
           		labileN_bud2labileN = labileC_bud2labile_Ra * (state%labileN_bud/state%labileC_bud)
           elseif(state%labileC_bud < -(state%labileC_Ra + flux%a_labileC_2Ra - flux%Ra_main).and. state%labileC_bud > 0.0 ) then
                 print *,'DEAD! - All labile C pools are 0 ',state%labileC
                 labileC_bud2labile_Ra = 0.0
                 labileN_bud2labileN = 0.0
           endif
      else
           labileC_bud2labile_Ra = 0.0
           labileN_bud2labileN = 0.0  
      endif
      
! -------  DISTURBANCE ---------------      

if(clim%doy(rstep) == 1) then
	do i = 1,io%dist_length
        if(site%distyear(i)  == cal_year) then

        	flux%leafC_c1_dist_atm = state%leafC_c1*site%distintensity(i)*site%distremove(i)
        	flux%leafC_c1_dist_litter = state%leafC_c1*site%distintensity(i)*(1-site%distremove(i))
        	
        	flux%leafC_c2_dist_atm = state%leafC_c2*site%distintensity(i)*site%distremove(i)
        	flux%leafC_c2_dist_litter = state%leafC_c2*site%distintensity(i)*(1-site%distremove(i))
        
        	flux%woodC_dist_atm = state%woodC*site%distintensity(i)*site%distremove(i)
			flux%woodC_dist_litter = state%woodC*site%distintensity(i)*(1-site%distremove(i))
			
        	flux%rootC_dist_atm = state%rootC*site%distintensity(i)*site%distremove(i)
        	flux%rootC_dist_litter = state%rootC*site%distintensity(i)*(1-site%distremove(i))
        	
        	flux%leafN_c1_dist_atm = state%leafN_c1*site%distintensity(i)*site%distremove(i)
        	flux%leafN_c1_dist_litter = state%leafN_c1*site%distintensity(i)*(1-site%distremove(i))
  
          	flux%leafN_c2_dist_atm = state%leafN_c2*site%distintensity(i)*site%distremove(i)
        	flux%leafN_c2_dist_litter = state%leafN_c2*site%distintensity(i)*(1-site%distremove(i))
        	      	
        	flux%woodN_dist_atm = state%woodN*site%distintensity(i)*site%distremove(i)
        	flux%woodN_dist_litter = state%woodN*site%distintensity(i)*(1-site%distremove(i))
        	
        	flux%rootN_dist_atm = state%rootN*site%distintensity(i)*site%distremove(i)
        	flux%rootN_dist_litter = state%rootN*site%distintensity(i)*(1-site%distremove(i))
        	
        	flux%labileC_dist_atm = state%labileC*site%distintensity(i)*site%distremove(i)
        	flux%labileC_dist_litter = state%labileC*site%distintensity(i)*(1-site%distremove(i))
        	
        	flux%labileC_bud_dist_atm = state%labileC_bud*site%distintensity(i)*site%distremove(i)
        	flux%labileC_bud_dist_litter = state%labileC_bud*site%distintensity(i)*(1-site%distremove(i))
        	
        	flux%labileC_Ra_dist_atm = state%labileC_Ra*site%distintensity(i)*site%distremove(i)
        	flux%labileC_Ra_dist_litter = state%labileC_Ra*site%distintensity(i)*(1-site%distremove(i))
        	
        	flux%labileN_dist_atm = state%labileN *site%distintensity(i)*site%distremove(i)
        	flux%labileN_dist_litter = state%labileN *site%distintensity(i)*(1-site%distremove(i))
        	
        	flux%labileN_bud_dist_atm = state%labileN_bud*site%distintensity(i)*site%distremove(i)
        	flux%labileN_bud_dist_litter = state%labileN_bud*site%distintensity(i)*(1-site%distremove(i))

        	state%maxleafC = state%maxleafC*(1-site%distintensity(i))
		    state%maxleafN = state%maxleafN *(1-site%distintensity(i))
		    state%maxrootC = state%maxrootC *(1-site%distintensity(i))
		    
		    leafC_dist = flux%leafC_c1_dist_litter + flux%leafC_c2_dist_litter
		    leafN_dist = flux%leafN_c1_dist_litter + flux%leafN_c2_dist_litter	    
		    	litterfallC = leafC_dist + flux%rootC_dist_litter + flux%labileC_dist_litter &
		    		+ flux%labileC_bud_dist_litter + flux%labileC_Ra_dist_litter + flux%woodC_dist_litter
      			litterfallN = leafN_dist + flux%rootN_dist_litter + flux%labileN_dist_litter &
		    		+ flux%labileN_bud_dist_litter + flux%woodN_dist_litter
 
      			rt = litterfallC/litterfallN
      			flux%dist_litterC_to_soil1 = litterfallC * max((param%aLF - param%bLF*param%flig*rt),param%flig_min)
      			flux%dist_litterC_to_soil2 = litterfallC * (1-max((param%aLF - param%bLF*param%flig*rt),param%flig_min))
      			flux%dist_litterN_to_soil1 = litterfallN*(max((param%aLF - param%bLF*param%flig*rt),param%flig_min) &
      				*min((rt/param%rls_min),1.))
      			flux%dist_litterN_to_soil2 = litterfallN*(1-(max((param%aLF - param%bLF* param%flig*rt),param%flig_min)&
      				*min((rt/param%rls_min),1.)))
		    
		   endif 
    end do    
    endif
    


!------ PRODUCTIVITY CALCULATIONS ------------

      ! NOTE: Ra_total is diagnostic because the individual fluxes are now 
      ! taken out of there respective labile pools
      flux%Ra_total = flux%Ra_Main + flux%Ra_Grow + flux%Ra_excessC 
      flux%NPP = flux%GPP - flux%Ra_total 
      flux%NEE = flux%npp - flux%Rh_total

!-------UPDATE STATE VARIABLES---------------------------

      !plant tissues
       
      state%leafC_c1 =  state%leafC_c1 + flux%a_labileC_bud_2leaf - &
                      flux%leafC_c1_to_c2 - flux%leafC_c1_dist_atm - flux%leafC_c1_dist_litter 
      state%leafC_c2 =  state%leafC_c2 + flux%leafC_c1_to_c2 - &
                      flux%t_leafC - flux%leafC_c2_dist_atm - flux%leafC_c2_dist_litter
    
      state%leafC   =  state%leafC_c1 + state%leafC_c2
                      
      state%woodC   = state%woodC + flux%a_woodC - flux%t_woodC - flux%woodC_dist_atm - flux%woodC_dist_litter
      
      state%rootC   = state%rootC + flux%a_rootC - flux%t_rootC - flux%rootC_dist_atm - flux%rootC_dist_litter

      state%leafN_c1   = state%leafN  + flux%a_labileN_bud_2leaf-  &
                      flux%leafN_c1_to_c2 - flux%leafN_c1_dist_atm - flux%leafN_c1_dist_litter
                      
      state%leafN_c1 =  state%leafN  + flux%leafN_c1_to_c2 - &
                      flux%t_leafN - flux%retransN - flux%leafN_c2_dist_atm - flux%leafN_c2_dist_litter
                      
      state%woodN   = state%woodN + flux%a_woodN - flux%t_woodN - flux%woodN_dist_atm - flux%woodN_dist_litter
      state%rootN   = state%rootN + flux%a_rootN - flux%t_rootN - flux%rootN_dist_atm - flux%rootN_dist_litter

      ! LABILE POOLS
      state%labileC = state%labileC + flux%GPP - flux%Ra_excessC - &
                      flux%a_labileC_bud  - flux%a_woodC - &
                      flux%a_rootC -flux%Ra_grow - flux%a_labileC_2Ra &
                      - flux%labileC_dist_atm - flux%labileC_dist_litter
                                      
      state%labileC_bud = state%labileC_bud + flux%a_labileC_bud - &
                      flux%a_labileC_bud_2leaf - labileC_bud2labile_Ra &
                      - flux%labileC_bud_dist_atm - flux%labileC_bud_dist_litter
                      
      state%labileC_Ra = state%labileC_Ra + flux%a_labileC_2Ra - &
                     flux%Ra_main + labileC_bud2labile_Ra &
                     - flux%labileC_Ra_dist_atm - flux%labileC_Ra_dist_litter
                         
      state%labileN = state%labileN + flux%no3_uptake + flux%nh4_uptake + & 
                     flux%retransN + flux%Nfix - flux%a_labileN_bud - &
                     flux%a_woodN - flux%a_rootN + labileN_bud2labileN  &
                      - flux%labileN_dist_atm - flux%labileN_dist_litter

      state%labileN_bud = state%labileN_bud + flux%a_labileN_bud - &
                      flux%a_labileN_bud_2leaf - labileN_bud2labileN  &
                       - flux%labileN_bud_dist_atm - flux%labileN_bud_dist_litter

      !dead organic matter

      state%soilC_1 = state%soilC_1 + flux%litterC_to_soil1 - flux%soil1_to_atm_C + &
      		flux%dist_litterC_to_soil1
      state%soilN_1 = state%soilN_1 + flux%litterN_to_soil1 - flux%soil1_to_NH4 + &
      		flux%dist_litterN_to_soil1
      state%soilC_2 = state%soilC_2 + flux%litterC_to_soil2 - flux%soil2_to_atm_C - &
      		flux%soil2_to_soil3_C - flux%soil2_to_soil4_C + flux%dist_litterC_to_soil2
      state%soilN_2 = state%soilN_2 + flux%litterN_to_soil2 - flux%soil2_to_NH4 + &
      		flux%dist_litterN_to_soil2
      state%soilC_3 = state%soilC_3 + flux%soil2_to_soil3_C - flux%soil3_to_atm_C -flux%leachDOC
      state%soilN_3 = state%soilN_3 + flux%soil2_to_soil3_N - flux%soil3_to_NH4 - flux%leachDON
      state%soilC_4 = state%soilC_4 + flux%soil2_to_soil4_C-  flux%soil4_to_atm_C
      state%soilN_4 = state%soilN_4 + flux%soil2_to_soil4_N - flux%soil4_to_NH4
      state%litterC = state%soilC_1 + state%soilC_2
      state%litterN = state%soilN_1 + state%soilN_2
      state%soilC = state%soilC_3 + state%soilC_4
      state%soilN = state%soilN_3 + state%soilN_4
      state%cwdC = 0
      state%cwdN = 0
      
      !inorganic pools
      state%nh4 = state%nh4 + flux%ndep_nh4 + gross_nmin - &
               flux%nh4_uptake - flux%nh4_immob - flux%nitr + flux%nfert_nh4
   
      state%no3 = state%no3 + flux%ndep_no3 + flux%nitr - & 
                flux%no3_uptake - flux%no3_immob - flux%leachN + flux%nfert_no3
! ---- UPDATE SUMMARY VARIABLES --------
    
      state%totvegc = state%leafC + state%woodC + state%rootC + & 
               state%labileC + state%labileC_bud + state%labileC_Ra
      state%totvegn = state%leafN + state%woodN + state%rootN + &
               state%labileN + state%labileN_bud

      state%totalC = state%totvegc + state%soilC_1 + state%soilC_2 + state%soilC_3 + state%soilC_4
      state%totalN = state%totvegn + state%soilN_1 + state%soilN_2 + state%soilN_3 + &
            	state%soilN_4  + state%nh4 + state%no3    
     
      flux%net_nmin =(gross_nmin -flux%nh4_immob - flux%no3_immob)

      ! SET CURRENT VEGETATION C:N RATIOS

      if(state%leafN > 0) then
        state%leafCN = state%leafC/state%leafN
      else
      	state%leafCN = state%target_leafCN
      endif

      state%woodCN = state%woodC/state%woodN
      state%rootCN = state%rootC/state%rootN 
      
      !SET MAX STORE SIZES
      state%maxNstore = (state%rootC + state%woodC) * param%Nlabile_prop
      state%MaxCstore = (state%rootC + state%woodC) * param%Clabile_prop
      state%maxRaC =    (state%rootC + state%woodC) * param%RaClabile_prop
    	  
      call year_update(0,year_count)

      pass = BalanceCheck()

      end subroutine ecosystem_dynamics

end module aconite_ecosystem 