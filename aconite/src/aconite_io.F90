      module aconite_io

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: 
     use aconite_type
! !PUBLIC TYPES:
      implicit none
      public :: read_climate
      public :: write_restart
      public :: read_restart
      public :: write_daily_output
      public :: write_annual_output
      public :: read_parameters
      public :: read_namelist
      public :: read_site_data
      public :: read_dist_data
      public :: read_nfert_data
      public :: read_soilwarm_data

!EOP
!-----------------------------------------------------------------------

     contains

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
   subroutine read_climate(clim,clim_length)

   implicit none

   integer :: clim_length
   real,pointer :: climate_data(:,:)
   integer:: nlines,i
   type (clim_type), intent(inout):: clim  
   character(len=200) tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10
!EOP
!------------------------------------------------------------------------

     allocate(climate_data(10,clim_length))
     open(11,file = io%clim_in, status = 'old')
     
     nlines = clim_length+1

     do i = 1,nlines
     if(i == 1) then
      		read(11,*) tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10
         else
             read(11,*) climate_data(1,i-1),climate_data(2,i-1),climate_data(3,i-1),climate_data(4,i-1),climate_data(5,i-1),&
             climate_data(6,i-1),climate_data(7,i-1),climate_data(8,i-1),climate_data(9,i-1),climate_data(10,i-1)
         endif
     end do
     close(11)

      allocate(clim%tmin(nlines))
      allocate(clim%tmax(nlines))
      allocate(clim%doy(nlines))
      allocate(clim%CO2(nlines))
      allocate(clim%rad(nlines))
      allocate(clim%prec(nlines))
      allocate(clim%year(nlines))
      allocate(clim%NO3dep(nlines))
      allocate(clim%NH4dep(nlines))

     clim%year(:) = climate_data(2,:)
     clim%doy(:) = climate_data(3,:)
     clim%rad(:) = climate_data(4,:)
     clim%prec(:) = climate_data(5,:)
     clim%tmax(:) = climate_data(6,:)
     clim%tmin(:) = climate_data(7,:)
     clim%NH4dep(:) = (climate_data(8,:)/365)
     clim%NO3dep(:) = climate_data(9,:)/365
     clim%CO2(:) = climate_data(10,:)
     deallocate(climate_data)
   
     
    end subroutine read_climate

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
   subroutine read_parameters

   implicit none

   real,pointer :: param_in(:)
   character(len=200) fname,tmp
   integer:: params,i
!EOP
!------------------------------------------------------------------------
     params = 86
     allocate(param_in(params))
     open(11,file = io%param_in, status = 'OLD')
     do i = 1,params
         read(11,*) tmp,param_in(i)
     end do
     close(11)

      param%root_radius= param_in(1)
      param%bufferNH4= param_in(2)
      param%diffNH4= param_in(3)
      param%bufferNO3= param_in(4)
      param%diffNO3 = param_in(5)
      param%imax= param_in(6)
      param%rooting_depth = param_in(7)
      param%vo = param_in(8)
      param%km= param_in(9)
      param%C_conc = param_in(10)
      param%root_density = param_in(11)
      param%a1= param_in(12)
      param%a2= param_in(13)
      param%a3= param_in(14)
      param%a4= param_in(15)
      param%a5= param_in(16)
      param%a6= param_in(17)
      param%a7= param_in(18)
      param%a8= param_in(19)
      param%a9= param_in(20)
      param%a10= param_in(21)
      param%lca = param_in(22)
      param%t_leaf = param_in(23)
      param%t_wood= param_in(24)
      param%t_root= param_in(25)
      param%t_excessC = param_in(26)
      param%t_cwd = param_in(27)
      param%trans_frac= param_in(28)
      param%t_litter= param_in(29)
      param%mCUE= param_in(30)
      param%t_soil= param_in(31)
      param%nitr_rate = param_in(32)
      param%leach_rate = param_in(33)
      param%Nturn_dep_loss = param_in(34)
      param%Ra_Q10 = param_in(35)
      param%Rh_Q10 = param_in(36)
      param%Ra_per_N = param_in(37)
      param%leaf_resp_A= param_in(38)
      param%leaf_resp_B= param_in(39)
      param%root_resp_A= param_in(40) !SAME AS LEAF
      param%root_resp_B= param_in(41) !SAME AS LEAF 
      param%Ra_grow= param_in(42)
      !param_in(43) ! NOT BEING USED
      param%soilCN = param_in(44)
      param%leafCN = param_in(45)
      param%woodCN = param_in(46)
      param%rootCN = param_in(47)
      param%GDDStart = param_in(48)
      param%SenescStart = param_in(49)
      param%Clabile_prop = param_in(50)
      param%Nlabile_prop = param_in(51)
      param%RaClabile_prop = param_in(52)
      param%add_C = param_in(53)
      param%Nfix_per_gC = param_in(54)
      !param_in(55) NOT BEING USED
      param%MaxAllocAdjust = param_in(56) 
      param%Minleaf2woodAlloc = param_in(57)
      param%LeafC2budCprop = param_in(58)
      !param_in(59) !NOT BEING USED
      param%leaf2root_ratio = param_in(60) 
      param%use_reich = param_in(61)
      param%stocks_close_prop = param_in(62)
      param%a11 = param_in(63)
      param%growth_potential_param  = param_in(64)
      param%leaf_shed_rate = param_in(65)
      param%aLF= param_in(66)
      param%bLF= param_in(67)
      param%flig= param_in(68)
      param%flig_min= param_in(69)
      param%rls_min= param_in(70)
      param%kLF= param_in(71)
      param%kLS= param_in(72)
      param%kSS= param_in(73)
      param%kSP= param_in(74)
      param%qmax= param_in(75)
      param%q_SP= param_in(76)
      param%r_SS= param_in(77)
      param%r_SP= param_in(78)
      param%k_S= param_in(79)
      param%xi= param_in(80)
      param%r_DOM= param_in(81)
      param%f_DOM= param_in(82)
      param%marg_ineff_exp= param_in(83)
      param%leafN_deficit_thres= param_in(84)
      param%soil_model = param_in(85)
      param%damp_rate = param_in(86)
      
     if(io%cold_start == 1) then
        state%leafN = state%leafC/param%leafCN
        state%target_leafCN = param%leafCN
        state%labileN_bud = state%labileC_bud/param%leafCN
        state%rootN = state%rootC/param%rootCN
        state%target_rootCN = param%rootCN
        state%woodN = state%woodC/param%woodCN
        state%cwdN= state%cwdC/param%woodCN
        state%soilN = state%soilC/param%soilCN
        state%totvegn_prev=state%leafN+state%woodN+state%rootN+state%labileN+state%labileN_bud
          if(param%soil_model==1)then
            state%totalN_prev = state%totvegn_prev + state%litterN + state%cwdN+ state%soilN + state%nh4 + state%no3  
          else
            state%totalN_prev = state%totvegn_prev + state%soilN_1 + state%soilN_2 + state%soilN_3 + &
            	state%soilN_4  + state%nh4 + state%no3    
          endif 
        state%MaxNstore = state%woodN * param%Nlabile_prop
        state%cwdN_prev=state%cwdN
        state%soilN_prev = state%soilN
        state%litterN_prev=state%litterN
        state%leafCN = state%leafC/state%leafN
        state%woodCN = state%woodC/state%woodN
        state%rootCN = state%rootC/state%rootN
     endif
  deallocate(param_in)
    end subroutine read_parameters

!------------------------------------------------------------------------

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
   subroutine read_site_data()

   implicit none

   real,pointer :: site_in(:)
   character(len=200) fname,tmp
   integer:: site_params,i
!EOP
!------------------------------------------------------------------------

     site_params = 2
     allocate(site_in(site_params))
     open(11,file = io%site_in, status = 'OLD')
     do i = 1,site_params
         read(11,*) tmp,site_in(i)
     end do
     close(11)

      site%Lat = site_in(1)
      site%seasonal = site_in(2)

     deallocate(site_in)
    end subroutine read_site_data
!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine read_dist_data()

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   real,pointer :: dist_data(:,:)
   integer:: nlines,i
   character(len=200) tmp1,tmp2,tmp3,tmp4
!EOP
!------------------------------------------------------------------------
     allocate(dist_data(4,io%dist_length))
     open(11,file = io%dist_in, status = 'old')

     do i = 1,io%dist_length+1
     	 if(i == 1) then
     	 	read(11,*) tmp1,tmp2,tmp3,tmp4
     	 else
            read(11,*) dist_data(1,i-1),dist_data(2,i-1),dist_data(3,i-1),dist_data(4,i-1)
         endif
    end do
     close(11)
     
     allocate(site%distyear(io%dist_length))
     allocate(site%distintensity(io%dist_length))
     allocate(site%distremove(io%dist_length))
     allocate(site%distsoilloss(io%dist_length))
     
     site%distyear(:) = dist_data(1,:)
     site%distintensity(:) = dist_data(2,:)
     site%distremove(:) = dist_data(3,:)
     site%distsoilloss(:) = dist_data(4,:)
    
     deallocate(dist_data)
    end subroutine read_dist_data
    
!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine read_nfert_data()

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   character(len=200) tmp1,tmp2,tmp3
   real,pointer :: nfert_data(:,:)
   integer:: nlines,i
!EOP
!------------------------------------------------------------------------
     allocate(nfert_data(3,io%nfert_length))
     open(11,file = io%nfert_in, status = 'old')

     do i = 1,io%nfert_length+1
         if(i == 1) then
         	read(11,*) tmp1,tmp2,tmp3
         else
            read(11,*) nfert_data(1,i-1),nfert_data(2,i-1),nfert_data(3,i-1)
         endif
         
     end do
     close(11)
     
     allocate(site%nfertyear(io%nfert_length))
     allocate(site%nfert_no3(io%nfert_length))
     allocate(site%nfert_nh4(io%nfert_length))
     
     site%nfertyear(:) = nfert_data(1,:)
     site%nfert_no3(:) = nfert_data(2,:)
     site%nfert_nh4(:) = nfert_data(3,:)
     
     deallocate(nfert_data)
    end subroutine read_nfert_data
    
!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
!
! !INTERFACE:
      subroutine read_soilwarm_data()

! !USES:
!
! !ARGUMENTS:
      implicit none
!
! LOCAL VARAIBLES:
   real,pointer :: soilwarm_data(:,:)
   integer:: nlines,i
   character(len=200) tmp1,tmp2
!EOP
!------------------------------------------------------------------------
     allocate(soilwarm_data(2,io%soilwarm_length))
     open(11,file = io%soilwarm_in, status = 'old')

     do i = 1,io%soilwarm_length+1
         if(i == 1) then
         	read(11,*) tmp1,tmp2
         else
           read(11,*) soilwarm_data(1,i-1),soilwarm_data(2,i-1)
         endif
    end do
     close(11)
     
     allocate(site%soilwarmyear(io%soilwarm_length))
     allocate(site%soilwarm_degree(io%soilwarm_length))

     
     site%soilwarmyear(:) = soilwarm_data(1,:)
     site%soilwarm_degree(:) = soilwarm_data(2,:)
    
     deallocate(soilwarm_data)
    end subroutine read_soilwarm_data

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
   subroutine write_restart()

   implicit none

   character(len=200) fname
   integer:: nlines,i,output_length
   real :: restart_out(114)
   character(len=200) restart_name(114)
!EOP
!------------------------------------------------------------------------

          output_length = 114

		  restart_out(1)= state%leafC 
		  restart_name(1) = 'leafC' 
          restart_out(2)=state%woodC
          restart_name(2) = 'woodC'
          restart_out(3)= state%rootC
          restart_name(3) = 'rootC'
          restart_out(4) = state%leafN
          restart_name(4) = 'leafN'
          restart_out(5) = state%woodN
          restart_name(5) = 'woodN'
          restart_out(6) = state%rootN
          restart_name(6) = 'rootN'
          restart_out(7) = state%labileC
          restart_name(7) = 'labileC' 
          restart_out(8) = state%labileN
          restart_name(8) = 'labileN'
          restart_out(9)= state%labileC_bud
          restart_name(9) = 'labileC_bud'
          restart_out(10) = state%labileC_Ra
          restart_name(10) = 'labileC_Ra'
          restart_out(11) = state%labileN_bud
          restart_name(11) = 'labileN_bud'          
          restart_out(12) = state%litterC
          restart_name(12) = 'litterC'           
          restart_out(13) = state%litterN
          restart_name(13) = 'litterN'
          restart_out(14) = state%soilC 
          restart_name(14) = 'soilC'
          restart_out(15) = state%soilN
          restart_name(15) = 'soilN'
          restart_out(16) = state%cwdC
          restart_name(16) = 'cwdC'
          restart_out(17) = state%cwdN
          restart_name(17) = 'cwdN'
          restart_out(18) = state%nh4
          restart_name(18) = 'nh4'
          restart_out(19) = state%no3
          restart_name(19) = 'no3'
          restart_out(20) = state%psid
          restart_name(20) = 'psid'
          restart_out(21) = state%rtot
          restart_name(21) = 'rtot'
          restart_out(22) =   state%Nuptake_downreg
          restart_name(22) = 'Nuptake_downreg'
          restart_out(23) = 0.0 
          restart_name(23) = 'NOT_USED'
          restart_out(24) = state%maxleafC
          restart_name(24) = 'maxleafC'
          restart_out(25) = state%hitmaxleafC 
          restart_name(25) = 'hitmaxleafC'
          restart_out(26) = state%maxrootC
          restart_name(26) = 'maxrootC'
          restart_out(27) = state%hitmaxrootC
          restart_name(27) = 'hitmaxrootC'
          restart_out(28) = state%maxRaC
          restart_name(28) = 'maxRaC'
          restart_out(29) = state%min_wood_deficit
          restart_name(29) = 'min_wood_deficit'          
          restart_out(30) = marg%integ_Creturn_leafC
          restart_name(30) = 'marg'
          restart_out(31) = marg%integ_Nreturn_leafC
          restart_name(31) = 'marg'
          restart_out(32) = marg%integ_Creturn_rootC
          restart_name(32) = 'marg'
          restart_out(33)= marg%integ_Nreturn_rootC
          restart_name(33) = 'marg'
          restart_out(34) = marg%integ_Creturn_Raexcess
          restart_name(34) = 'marg'
          restart_out(35) = marg%integ_Nreturn_Raexcess
          restart_name(35) = 'marg'
          restart_out(36) = marg%integ_Creturn_leafN
          restart_name(36) = 'marg'
          restart_out(37) = marg%integ_Nreturn_leafN
          restart_name(37) = 'marg'
          restart_out(38) = marg%integ_Creturn_rootN
          restart_name(38) = 'marg'
          restart_out(39)= marg%integ_Nreturn_rootN
          restart_name(39) = 'marg'
          restart_out(40) = 0.0 !NOT USED
          restart_name(40) = 'NOT_USED'
          restart_out(41) = 0.0 !NOT USED
          restart_name(41) = 'NOT_USED'
          restart_out(42) = 0.0 !NOT USED
          restart_name(42) = 'NOT_USED'
          restart_out(43) = 0.0 !NOT USED
          restart_name(43) = 'NOT_USED'
          restart_out(44) = marg%integ_Creturn_leafCN
          restart_name(44) = 'marg'
          restart_out(45) = marg%integ_Nreturn_rootCN
          restart_name(45) = 'marg'
          restart_out(46) = marg%integ_GPP_leafC
          restart_name(46) = 'marg'
          restart_out(47) = marg%integ_Rm_leafC
          restart_name(47) = 'marg'
          restart_out(48) = marg%integ_Rg_leafC
          restart_name(48) = 'marg'
          restart_out(49) = marg%integ_GPP_leafN
          restart_name(49) = 'marg'
          restart_out(50) = marg%integ_addN_leafN
          restart_name(50) = 'marg'
          restart_out(51) = marg%integ_Rm_leafN
          restart_name(51) = 'marg'
          restart_out(52) = marg%integ_GPP_leafCN
          restart_name(52) = 'marg'
          restart_out(53) = marg%integ_Rm_leafCN
          restart_name(53) = 'marg'
          restart_out(54) = marg%integ_Rm_rootC
          restart_name(54) = 'marg'
          restart_out(55) = marg%integ_Rg_rootC
          restart_name(55) = 'marg'
          restart_out(56)  = marg%integ_Nuptake_rootC
          restart_name(56) = 'marg'
          restart_out(57) = marg%integ_Rm_rootN
          restart_name(57) = 'marg'
          restart_out(58) = marg%integ_Nuptake_rootN
          restart_name(58) = 'marg'
          restart_out(59) = marg%integ_addN_rootN 
          restart_name(59) = 'marg'       
          restart_out(60) = marg%integ_Nuptake_rootCN
          restart_name(60) = 'marg'   
          restart_out(61) = 0.0 !NOT USED
          restart_name(61) = 'NOT_USED'
          restart_out(62) = marg%integ_Nreturn_Raexcess
          restart_name(62) = 'marg'   
          restart_out(63) = marg%annual_Creturn_leafC
          restart_name(63) = 'marg'   
          restart_out(64) = marg%annual_Nreturn_leafC
          restart_name(64) = 'marg'   
          restart_out(65) = marg%annual_Creturn_rootC
          restart_name(65) = 'marg'   
          restart_out(66) = marg%annual_Nreturn_rootC
          restart_name(66) = 'marg'   
          restart_out(67) = marg%annual_Creturn_Raexcess
          restart_name(67) = 'marg'   
          restart_out(68) = marg%annual_Nreturn_Raexcess
          restart_name(68) = 'marg'   
          restart_out(69) = marg%annual_Creturn_leafN
          restart_name(69) = 'marg'   
          restart_out(70) = marg%annual_Nreturn_leafN
          restart_name(70) = 'marg'   
          restart_out(71) = marg%annual_Creturn_rootN
          restart_name(71) = 'marg'   
          restart_out(72) = marg%annual_Nreturn_rootN
          restart_name(72) = 'marg'   
          restart_out(73) = 0.0 !NOT USED
          restart_name(73) = 'NOT_USED'
          restart_out(74) = 0.0 !NOT USED
          restart_name(74) = 'NOT_USED'
          restart_out(75) = 0.0 !NOT USED
          restart_name(75) = 'NOT_USED'
          restart_out(76) = 0.0 !NOT USED
          restart_name(76) = 'NOT_USED'
          restart_out(77) = marg%annual_Creturn_leafCN
          restart_name(77) = 'marg' 
          restart_out(78) = marg%annual_Nreturn_rootCN
          restart_name(78) = 'marg' 
          restart_out(79) = marg%annual_GPP_leafC
          restart_name(79) = 'marg' 
          restart_out(80) = marg%annual_Rm_leafC
          restart_name(80) = 'marg' 
          restart_out(81) = marg%annual_Rg_leafC
          restart_name(81) = 'marg' 
          restart_out(82) = marg%annual_GPP_leafN
          restart_name(82) = 'marg' 
          restart_out(83) = marg%annual_addN_leafN
          restart_name(83) = 'marg' 
          restart_out(84) = marg%annual_Rm_leafN
          restart_name(84) = 'marg' 
          restart_out(85) = marg%annual_GPP_leafCN
          restart_name(85) = 'marg' 
          restart_out(86) = marg%annual_Rm_leafCN
          restart_name(86) = 'marg' 
          restart_out(87) = marg%annual_Rm_rootC
          restart_name(87) = 'marg' 
          restart_out(88) = marg%annual_Rg_rootC
          restart_name(88) = 'marg' 
          restart_out(89) = marg%annual_Nuptake_rootC
          restart_name(89) = 'marg' 
          restart_out(90) = marg%annual_Rm_rootN
          restart_name(90) = 'marg' 
          restart_out(91) = marg%annual_Nuptake_rootN
          restart_name(91) = 'marg' 
          restart_out(92) = marg%annual_addN_rootN  
          restart_name(92) = 'marg'      
          restart_out(93) = marg%annual_Nuptake_rootCN
          restart_name(93) = 'marg' 
          restart_out(94) = 0.0 !NOT USED
          restart_name(94) = 'NOT_USED'
          restart_out(95) = marg%annual_Nreturn_Raexcess
          restart_name(95) = 'marg' 
          restart_out(96) = 0.0
          restart_name(96) = 'NOT_USED'
          restart_out(97) = 0.0
          restart_name(97) = 'NOT_USED'
          restart_out(98) = 0.0
          restart_name(98) = 'NOT_USED'
          restart_out(99) = 0.0
          restart_name(99) = 'NOT_USED'
          restart_out(100) = marg%day_count_leaf
          restart_name(100) = 'marg' 
          restart_out(101) = marg%integ_hitmaxleafC
          restart_name(101) = 'marg' 
          restart_out(102) = marg%integ_hitmaxrootC
          restart_name(102) = 'marg' 
          restart_out(103) = state%soilC_1
          restart_name(103) = 'soilC_1' 
          restart_out(104) = state%soilC_2
          restart_name(104) = 'soilC_2' 
          restart_out(105) = state%soilC_3
          restart_name(105) = 'soilC_3' 
          restart_out(106) = state%soilC_4
          restart_name(106) = 'soilC_4'
          restart_out(107) = state%soilN_1
          restart_name(107) = 'soilN_1'
          restart_out(108) = state%soilN_2
          restart_name(108) = 'soilN_2'
          restart_out(109) = state%soilN_3
          restart_name(109) = 'soilN_3'
          restart_out(110) = state%soilN_4
          restart_name(110) = 'soilN_4'
          restart_out(111) = state%leafN_deficit
          restart_name(111) = 'leafN_deficit'
          restart_out(112)= state%leafCN
          restart_name(112) = 'leafCN'
          restart_out(113) = state%maxleafN
          restart_name(113) = 'maxleafN'
          restart_out(114) = state%target_leafCN
          restart_name(114) = 'maxleafN'
          
        OPEN (7, FILE = io%restart_out, STATUS = 'UNKNOWN')
        do i = 1,output_length
          write(7,'(A11,1x,A,1x,F16.8)') restart_name(i),',',restart_out(i)
          !write (7,100) restart_name(i),restart_out(i)                   
          !100 format (A10,',',1x,F16.8)
       end do
       CLOSE(7)         

      end subroutine write_restart
!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 

   subroutine read_restart()

   implicit none

   character(len=200) fname,tmp
   integer:: nlines,i,input_length
   real :: restart_in(114)
!EOP
!------------------------------------------------------------------------

    input_length = 114
    open(7,file = io%restart_in, STATUS = 'OLD')
    
     do i = 1,input_length
         read(7,*) tmp,restart_in(i)
     end do
    close(7)

          state%leafC= restart_in(1)
          state%woodC =restart_in(2)
          state%rootC= restart_in(3)
          state%leafN= restart_in(4)
          state%woodN= restart_in(5)
          state%rootN= restart_in(6) 
          state%labileC= restart_in(7)
          state%labileN= restart_in(8)
          state%labileC_bud=restart_in(9)
          state%labileC_Ra= restart_in(10)
          state%labileN_bud= restart_in(11)
          state%totvegc_prev=state%leafC+state%woodC+state%rootC+state%labileC+ state%labileC_bud + &
               state%labileC_Ra
          state%totvegn_prev=state%leafN+state%woodN+state%rootN+state%labileN+state%labileN_bud
          state%litterC=restart_in(12)
          state%litterN=restart_in(13)
          state%soilC= restart_in(14)
          state%soilN= restart_in(15)
          state%cwdC= restart_in(16)
          state%cwdN= restart_in(17)
          state%nh4=restart_in(18)
          state%no3=restart_in(19)
          state%psid=restart_in(20)
          state%rtot=restart_in(21)
          state%Nuptake_downreg = restart_in(22)
          !state%Cuptake_downreg = restart_in(23)
          state%maxleafC = max(restart_in(24),state%leafC)
          state%hitmaxleafC = restart_in(25)
          state%maxrootC = max(restart_in(26),state%rootC)
          state%hitmaxrootC = restart_in(27)
          state%maxRaC = restart_in(28)
          state%maxRaC =  max(restart_in(28),state%labileC_Ra)
          state%min_wood_deficit = restart_in(29)
          marg%integ_Creturn_leafC=restart_in(30)
          marg%integ_Nreturn_leafC=restart_in(31)
          marg%integ_Creturn_rootC=restart_in(32)
          marg%integ_Nreturn_rootC=restart_in(33)
          marg%integ_Creturn_Raexcess=restart_in(34)
          marg%integ_Nreturn_Raexcess=restart_in(35)
          marg%integ_Creturn_leafN=restart_in(36)
          marg%integ_Nreturn_leafN=restart_in(37)
          marg%integ_Creturn_rootN=restart_in(38)
          marg%integ_Nreturn_rootN=restart_in(39)
          !restart_in(40)  NOT CURRENTLY USED
          !restart_in(41)  NOT CURRENTLY USED
          !restart_in(42)  NOT CURRENTLY USED
          !restart_in(43)  NOT CURRENTLY USED
          marg%integ_Creturn_leafCN=restart_in(44)
          marg%integ_Nreturn_rootCN=restart_in(45)
          marg%integ_GPP_leafC=restart_in(46)
          marg%integ_Rm_leafC=restart_in(47)
          marg%integ_Rg_leafC=restart_in(48)
          marg%integ_GPP_leafN=restart_in(49)
          marg%integ_addN_leafN=restart_in(50)
          marg%integ_Rm_leafN=restart_in(51)
          marg%integ_GPP_leafCN=restart_in(52)
          marg%integ_Rm_leafCN=restart_in(53)
          marg%integ_Rm_rootC=restart_in(54)
          marg%integ_Rg_rootC=restart_in(55)
          marg%integ_Nuptake_rootC=restart_in(56) 
          marg%integ_Rm_rootN=restart_in(57)
          marg%integ_Nuptake_rootN=restart_in(58)  
          marg%integ_addN_rootN=restart_in(59)             
          marg%integ_Nuptake_rootCN=restart_in(60)    
          !restart_in(61)  NOT CURRENTLY USED
          marg%integ_Nreturn_Raexcess=restart_in(62)
          marg%annual_Creturn_leafC=restart_in(63)
          marg%annual_Nreturn_leafC=restart_in(64)
          marg%annual_Creturn_rootC=restart_in(65)
          marg%annual_Nreturn_rootC=restart_in(66)
          marg%annual_Creturn_Raexcess=restart_in(67)
          marg%annual_Nreturn_Raexcess=restart_in(68)
          marg%annual_Creturn_leafN=restart_in(69)
          marg%annual_Nreturn_leafN=restart_in(70)
          marg%annual_Creturn_rootN=restart_in(71)
          marg%annual_Nreturn_rootN=restart_in(72)
          !restart_in(73)  NOT CURRENTLY USED
          !restart_in(74)  NOT CURRENTLY USED
          !restart_in(75)  NOT CURRENTLY USED
          !restart_in(76)  NOT CURRENTLY USED
          marg%annual_Creturn_leafCN=restart_in(77)
          marg%annual_Nreturn_rootCN=restart_in(78)
          marg%annual_GPP_leafC=restart_in(79)
          marg%annual_Rm_leafC=restart_in(80)
          marg%annual_Rg_leafC=restart_in(81)
          marg%annual_GPP_leafN=restart_in(82)
          marg%annual_addN_leafN=restart_in(83)
          marg%annual_Rm_leafN=restart_in(84)
          marg%annual_GPP_leafCN=restart_in(85)
          marg%annual_Rm_leafCN=restart_in(86)
          marg%annual_Rm_rootC=restart_in(87)
          marg%annual_Rg_rootC=restart_in(88)
          marg%annual_Nuptake_rootC  =restart_in(89)
          marg%annual_Rm_rootN=restart_in(90)
          marg%annual_Nuptake_rootN   =restart_in(91)
          marg%annual_addN_rootN    =restart_in(92)         
          marg%annual_Nuptake_rootCN =restart_in(93)  
          !restart_in(94) NOT CURRENTLY USED
          marg%annual_Nreturn_Raexcess=restart_in(95) 
          !restart_in(96)
          !restart_in(97)
          !restart_in(98)
          !restart_in(99)
          marg%day_count_leaf  =restart_in(100)
          marg%integ_hitmaxleafC =restart_in(101)
          marg%integ_hitmaxrootC = restart_in(102)
          state%soilC_1 = restart_in(103)
          state%soilC_2 = restart_in(104)
          state%soilC_3 = restart_in(105)
          state%soilC_4 = restart_in(106) 
          state%soilN_1 = restart_in(107)
          state%soilN_2 = restart_in(108) 
          state%soilN_3 = restart_in(109) 
          state%soilN_4 = restart_in(110) 
          state%leafN_deficit = restart_in(111) 
          state%leafCN = restart_in(112)
          state%maxleafN = restart_in(113)
          state%target_leafCN = restart_in(114)
          
         if(param%soil_model==1)then
            state%totalC_prev = state%totvegc_prev + state%litterC + state%cwdC + state%soilC
            state%totalN_prev = state%totvegn_prev + state%litterN + state%cwdN+ state%soilN + state%nh4 + state%no3  
          else
            state%totalC_prev = state%totvegc_prev + state%soilC_1 + state%soilC_2 + state%soilC_3 + state%soilC_4
            state%totalN_prev = state%totvegn_prev + state%soilN_1 + state%soilN_2 + state%soilN_3 + &
            	state%soilN_4  + state%nh4 + state%no3    
          endif 
          
          state%litterC_prev=state%litterC
          state%cwdc_prev=state%cwdC
          state%soilC_prev = state%soilC
          state%litterN_prev=state%litterN
          state%cwdN_prev=state%cwdN
          state%soilN_prev = state%soilN
          state%minN_prev = state%nh4 + state%no3
          state%woodCN = state%woodC/state%woodN
          state%rootCN = state%rootC/state%rootN
         ! if(state%leafC > 0) then
         ! 		state%leafCN = state%leafC/state%leafN
         ! else
         ! 		state%leafCN = state%labileC_bud/state%labileN_bud
         ! endif
          
          state%target_rootCN = param%rootCN
          state%MaxNstore = state%woodN * param%Nlabile_prop
          state%MaxCstore = state%rootC * param%Clabile_prop
          
      end subroutine read_restart

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
   subroutine write_daily_output(model_year,mstep,cal_year)

   implicit none


   character(len=100),dimension(75) :: header
   real :: daily_out(75)
   integer :: model_year
   integer :: mstep
   integer :: cal_year
!EOP
!------------------------------------------------------------------------

daily_out(1) = (mstep/365.0)
header(1) = 'MODEL_YEAR'
daily_out(2) = cal_year
header(2) = 'CAL_YEAR'
daily_out(3) = state%lai 
header(3) = 'LAI'
daily_out(4) = state%leafC
header(4) = 'LEAF_C'
daily_out(5) = state%woodC
header(5) = 'WOOD_C'
daily_out(6) = state%rootC
header(6) = 'ROOT_C'
daily_out(7) = state%labileC
header(7) = 'LABILE_C'
daily_out(8) = state%labileC_bud
header(8) = 'LABILE_C_BUD'
daily_out(9) = state%labileC_Ra
header(9) = 'LABILE_C_RA'
daily_out(10) = state%leafN
header(10) = 'LEAF_N'
daily_out(11) = state%woodN
header(11) = 'WOOD_N'
daily_out(12) = state%rootN
header(12) = 'ROOT_N'
daily_out(13) = state%labileN
header(13) = 'LABILE_N'
daily_out(14) = state%labileN_bud
header(14) = 'LABILE_N_BUD'
daily_out(15) = state%totvegc
header(15) = 'TOTVEGC'
daily_out(16) = state%totvegn
header(16) = 'TOTVEGN'
daily_out(17) = state%cwdC
header(17) = 'CWDC'
daily_out(18) = state%cwdN
header(18) = 'CWDN'
daily_out(19) = state%litterC
header(19) = 'LITTER_C'
daily_out(20) = state%litterN
header(20) =  'LITTER_N'
daily_out(21) = state%soilC
header(21) = 'SOIL_C'
daily_out(22) = state%soilN
header(22) = 'SOIL_N'
daily_out(23) = state%nh4
header(23) = 'SOIL_NH4'
daily_out(24) = state%no3
header(24) = 'SOIL_NO3'
daily_out(25) = flux%GPP
header(25) = 'GPP'
daily_out(26) = flux%NPP
header(26) = 'NPP'
daily_out(27) = flux%NEE
header(27) = 'NEE'
daily_out(28) = flux%Rh_total
header(28) = 'RH'
daily_out(29) =flux%no3_uptake + flux%nh4_uptake
header(29) =  'PLANT_NUPTAKE'
daily_out(30) = flux%nh4_immob + flux%no3_immob
header(30) = 'N_IMMOBOLIZATION'
daily_out(31) = flux%net_nmin
header(31) = 'NET_N_MIN'
daily_out(32) = flux%leachN
header(32) = 'NO3_LEACHING'
daily_out(33) = state%leafCN
header(33) = 'LEAF_CN'
daily_out(34) = state%woodCN
header(34) = 'WOOD_CN'
daily_out(35) = state%rootCN
header(35) = 'ROOT_CN'
daily_out(36) = flux%a_labileC_bud_2leaf
header(36) = 'ALLOCATION_TO_LEAFC'
daily_out(37) = flux%a_woodC
header(37) = 'ALLOCATION_TO_WOODC'
daily_out(38) = flux%a_rootC
header(38) = 'ALLOCATION_TO_ROOTC'
daily_out(39) = flux%a_labileN_bud_2leaf
header(39) = 'ALLOCATION_TO_LEAFN'
daily_out(40) = flux%a_woodN
header(40) = 'ALLOCATION_TO_WOODN'
daily_out(41) = flux%a_rootN
header(41) = 'ALLOCATION_TO_ROOTN'
daily_out(42) = flux%Nfix
header(42) = 'N_FIXATION'
daily_out(43) = flux%nitr
header(43) =  'NITRIFICATION'
daily_out(44) = flux%t_leafC
header(44) =  'TURNOVER_LEAF_C'
daily_out(45) = flux%t_woodC
header(45) = 'TURNOVER_WOOD_C'
daily_out(46) = flux%t_rootC
header(46) = 'TURNOVER_ROOT_C'
daily_out(47) = flux%t_leafN
header(47) = 'TURNOVER_LEAF_N'
daily_out(48) = flux%t_woodN
header(48) = 'TURNOVER_WOOD_N'
daily_out(49) = flux%t_rootN
header(49) = 'TURNOVER_ROOT_N'
daily_out(50) = flux%Ra_total
header(50) = 'RA'
daily_out(51) = flux%Ra_grow
header(51) = 'RA_GROWTH'
daily_out(52) = flux%Ra_main
header(52) = 'RA_MAINT'
daily_out(53) = flux%Ra_excessC
header(53) = 'RA_EXCESS'
daily_out(54) = flux%retransN
header(54) = 'RETRANSLOCATED_N'
daily_out(55) = state%MaxCstore
header(55) = 'MAX_C_STORE'
daily_out(56) = state%MaxNstore
header(56) = 'MAX_N_STORE'
daily_out(57) = state%Nuptake_downreg
header(57) = 'N_UPTAKE_DOWNREG'
daily_out(58) = marg%integ_Creturn_leafC
header(58) = 'Creturn_leafC'
daily_out(59) = marg%integ_Nreturn_leafC
header(59) = 'Creturn_rootC'
daily_out(60) = marg%integ_Nreturn_rootC
header(60) = 'Nreturn_rootC'
daily_out(61) = marg%integ_Nreturn_Raexcess
header(61) = 'Nreturn_Raexcess'
daily_out(62) = marg%integ_Creturn_leafN
header(62) = 'Creturn_leafN'
daily_out(63) = marg%integ_Nreturn_leafN
header(63) = 'Nreturn_rootN'
daily_out(64) = marg%integ_Creturn_leafCN
header(64) = 'Creturn_leafCN'
daily_out(65) = marg%integ_Nreturn_rootCN
header(65) = 'Nreturn_rootCN'
daily_out(66) = state%labileC_bud
header(66) = 'LABILE_C_BUD'
daily_out(67) = state%labileN_bud
header(67) = 'lABILE_N_BUD'
daily_out(68) = state%labileC_Ra
header(68) = 'LABILE_C_RA'
daily_out(69) = state%target_leafCN
header(69) = 'TARGET_LEAFCN'
daily_out(70) = state%target_rootCN
header(70) = 'MAX_LEAF_C'
daily_out(71) = state%maxrootC
header(71) = 'MAX_ROOT_C'
daily_out(72) = state%debug
header(72) = 'DEBUG'
daily_out(73) = state%debug2
header(73) = 'DEBUG_2'
daily_out(74) = flux%leachDON
header(74) = 'DON_LEACHING'
daily_out(75) = flux%ndep_nh4 + flux%ndep_no3
header(75) = 'N_DEPOSITION'

open(7,file = io%day_out, status='OLD',ACCESS = 'APPEND')
if(mstep == ((io%spinup_length+io%print_year_start)*365)) then
    write(7,'(75(A23))'),header(:)
endif
write(7,'(75(G12.4))') daily_out(:)
close(7)
end subroutine write_daily_output


!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
      subroutine write_annual_output(rstep,model_year,mstep,cal_year)

      implicit none

   character(len=100),dimension(75) :: header
   real :: annual_out(75)
   integer :: rstep
   integer :: model_year
   integer :: mstep
   integer :: cal_year
!EOP
 !------------------------------------------------------------------------
annual_out(1) = (mstep/365.0)
header(1) = 'MODEL_YEAR'
annual_out(2) = cal_year
header(2) = 'CAL_YEAR'
annual_out(3) = state%annual_lai 
header(3) = 'LAI'
annual_out(4) = state%annual_leafC
header(4) = 'LEAF_C'
annual_out(5) = state%annual_woodC
header(5) = 'WOOD_C'
annual_out(6) = state%annual_rootC
header(6) = 'ROOT_C'
annual_out(7) = state%annual_labileC
header(7) = 'LABILE_C'
annual_out(8) = state%annual_labileC_bud
header(8) = 'LABILE_C_BUD'
annual_out(9) = state%annual_labileC_Ra
header(9) = 'LABILE_C_RA'
annual_out(10) = state%annual_leafN
header(10) = 'LEAF_N'
annual_out(11) = state%annual_woodN
header(11) = 'WOOD_N'
annual_out(12) = state%annual_rootN
header(12) = 'ROOT_N'
annual_out(13) = state%annual_labileN
header(13) = 'LABILE_N'
annual_out(14) = state%annual_labileN_bud
header(14) = 'LABILE_N_BUD'
annual_out(15) = state%annual_totvegc
header(15) = 'TOTVEGC'
annual_out(16) = state%annual_totvegn
header(16) = 'TOTVEGN'
annual_out(17) = state%annual_cwdC
header(17) = 'CWDC'
annual_out(18) = state%annual_cwdN
header(18) = 'CWDN'
annual_out(19) = state%annual_litterC
header(19) = 'LITTER_C'
annual_out(20) = state%annual_litterN
header(20) =  'LITTER_N'
annual_out(21) = state%annual_soilC
header(21) = 'SOIL_C'
annual_out(22) = state%annual_soilN
header(22) = 'SOIL_N'
annual_out(23) = state%annual_nh4
header(23) = 'SOIL_NH4'
annual_out(24) = state%annual_no3
header(24) = 'SOIL_NO3'
annual_out(25) = flux%annual_GPP
header(25) = 'GPP'
annual_out(26) = flux%annual_NPP
header(26) = 'NPP'
annual_out(27) = flux%annual_NEE
header(27) = 'NEE'
annual_out(28) = flux%annual_Rh
header(28) = 'RH'
annual_out(29) = flux%annual_Nuptake
header(29) =  'PLANT_NUPTAKE'
annual_out(30) = flux%annual_immob
header(30) = 'N_IMMOBOLIZATION'
annual_out(31) = flux%annual_netNmin
header(31) = 'NET_N_MIN'
annual_out(32) = flux%annual_leachN
header(32) = 'NO3_LEACHING'
annual_out(33) = state%annual_leafCN
header(33) = 'LEAF_CN'
annual_out(34) = state%annual_woodCN
header(34) = 'WOOD_CN'
annual_out(35) = state%annual_rootCN
header(35) = 'ROOT_CN'
annual_out(36) = flux%annual_a_leafC
header(36) = 'ALLOCATION_TO_LEAFC'
annual_out(37) = flux%annual_a_woodC
header(37) = 'ALLOCATION_TO_WOODC'
annual_out(38) = flux%annual_a_rootC
header(38) = 'ALLOCATION_TO_ROOTC'
annual_out(39) = flux%annual_a_leafN
header(39) = 'ALLOCATION_TO_LEAFN'
annual_out(40) = flux%annual_a_woodN
header(40) = 'ALLOCATION_TO_WOODN'
annual_out(41) = flux%annual_a_rootN
header(41) = 'ALLOCATION_TO_ROOTN'
annual_out(42) = flux%annual_Nfix
header(42) = 'N_FIXATION'
annual_out(43) = flux%annual_nitr
header(43) =  'NITRIFICATION'
annual_out(44) = flux%annual_t_leafC
header(44) =  'TURNOVER_LEAF_C'
annual_out(45) = flux%annual_t_woodC
header(45) = 'TURNOVER_WOOD_C'
annual_out(46) = flux%annual_t_rootC
header(46) = 'TURNOVER_ROOT_C'
annual_out(47) = flux%annual_t_leafN
header(47) = 'TURNOVER_LEAF_N'
annual_out(48) = flux%annual_t_woodN
header(48) = 'TURNOVER_WOOD_N'
annual_out(49) = flux%annual_t_rootN
header(49) = 'TURNOVER_ROOT_N'
annual_out(50) = flux%annual_Ra
header(50) = 'RA'
annual_out(51) = flux%annual_Ra_grow
header(51) = 'RA_GROWTH'
annual_out(52) = flux%annual_Ra_main
header(52) = 'RA_MAINT'
annual_out(53) = flux%annual_Ra_excessC
header(53) = 'RA_EXCESS'
annual_out(54) = flux%annual_retransN
header(54) = 'RETRANSLOCATED_N'
annual_out(55) = state%annual_MaxCstore
header(55) = 'MAX_C_STORE'
annual_out(56) = state%annual_MaxNstore
header(56) = 'MAX_N_STORE'
annual_out(57) = state%annual_Nuptake_downreg
header(57) = 'N_UPTAKE_DOWNREG'
annual_out(58) = marg%integ_Creturn_leafC
header(58) = 'Creturn_leafC'
annual_out(59) = marg%integ_Nreturn_leafC
header(59) = 'Creturn_rootC'
annual_out(60) = marg%integ_Nreturn_rootC
header(60) = 'Nreturn_rootC'
annual_out(61) = marg%integ_Nreturn_Raexcess
header(61) = 'Nreturn_Raexcess'
annual_out(62) = marg%integ_Creturn_leafN
header(62) = 'Creturn_leafN'
annual_out(63) = marg%integ_Nreturn_leafN
header(63) = 'Nreturn_rootN'
annual_out(64) = marg%integ_Creturn_leafCN
header(64) = 'Creturn_leafCN'
annual_out(65) = marg%integ_Nreturn_rootCN
header(65) = 'Nreturn_rootCN'
annual_out(66) = state%annual_labileC_bud
header(66) = 'LABILE_C_BUD'
annual_out(67) = state%annual_labileN_bud
header(67) = 'lABILE_N_BUD'
annual_out(68) = state%annual_labileC_Ra
header(68) = 'LABILE_C_RA'
annual_out(69) = state%annual_targetleafCN
header(69) = 'TARGET_LEAFCN'
annual_out(70) = state%annual_targetrootCN
header(70) = 'MAX_LEAF_C'
annual_out(71) = state%maxrootC
header(71) = 'MAX_ROOT_C'
annual_out(72) = state%debug
header(72) = 'DEBUG'
annual_out(73) = state%debug2
header(73) = 'DEBUG_2'
annual_out(74) = flux%annual_leachDON
header(74) = 'DON_LEACHING'
annual_out(75) = flux%annual_Ndep 
header(75) = 'N_DEPOSITION'

open(7,file = io%annual_out, status='OLD',ACCESS = 'APPEND')
if(mstep == ((io%spinup_length+io%print_year_start)*365)) then
    write(7,'(75(A23))'),header(:)
endif
write(7,'(75(G12.4))') annual_out(:)
close(7)
end subroutine write_annual_output

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: 
      subroutine read_namelist()

      implicit none

   character(len=200) clim_file
   character(len=200) param_file
   character(len=200) day_out
   character(len=200) annual_out
   character(len=200) restart_in
   character(len=200) restart_out
   character(len=200) site_in
   character(len=200) dist_in
   character(len=200) nfert_in
   character(len=200) soilwarm_in   
   integer :: sim_length
   integer :: clim_length
   integer :: dist_length
   integer :: nfert_length
   integer :: soilwarm_length
   integer :: print_year_start
   integer :: print_year_end
   integer :: annual_state_doy
   integer :: sim_start_year
   integer :: cold_start
   integer :: spinup_length
   integer :: spinup_clim_length
   namelist /ACONITE_IN/CLIM_FILE,PARAM_FILE,DAY_OUT,ANNUAL_OUT,RESTART_IN,RESTART_OUT,SITE_IN,&
       DIST_IN,NFERT_IN,SOILWARM_IN,SIM_LENGTH,CLIM_LENGTH,DIST_LENGTH,NFERT_LENGTH,SOILWARM_LENGTH,&
       PRINT_YEAR_START,PRINT_YEAR_END,ANNUAL_STATE_DOY,SIM_START_YEAR,COLD_START,SPINUP_LENGTH,&
       SPINUP_CLIM_LENGTH
      
!EOP
!------------------------------------------------------------------------
print *,'READING NAMELIST:'

open(8,file='aconite_namelist', status='OLD')
read(8,nml = ACONITE_IN) 
    io%clim_in = clim_file
    io%param_in = param_file
    io%day_out = day_out
    io%annual_out = annual_out
    io%site_in = site_in
    io%dist_in = dist_in
    io%nfert_in = nfert_in
    io%soilwarm_in = soilwarm_in
    io%restart_in = restart_in
    io%restart_out = restart_out
    io%sim_length = sim_length
    io%clim_length = clim_length
    io%dist_length = dist_length
    io%nfert_length = nfert_length
    io%soilwarm_length = soilwarm_length
    io%print_year_start = print_year_start
    io%print_year_end = print_year_end
    io%annual_state_doy = annual_state_doy
    io%sim_start_year= sim_start_year
    io%cold_start = cold_start
    io%spinup_length = spinup_length
    io%spinup_clim_length = spinup_clim_length
close(8)
if(io%clim_in == '') then
   print *, 'CLIMATE FILE: WARNING: NEEDS CLIMATE DATA'
else
   print *,'CLIMATE FILE:  ', io%clim_in
endif

if(io%param_in == '') then
    print *,'PARAMETER FILE: NO FILE PROVIDED USING DEFAULTS '
else
    print *, 'PARAMETER FILE:  ', io%param_in
endif
if(io%site_in == '') then
    print *,'SITE INPUT FILE: NO SITE FILE LISTED USING DEFAULTS'
else
	print *, 'SITE INPUT FILE:  ', io%site_in
endif

if(io%dist_in == '') then
   print *, 'DISTURBANCE FILE: NO DISTURBANCE DATA USED'
else
   print *, 'DISTURBANCE FILE:  ', io%dist_in
endif

if(io%nfert_in == '') then
   print *, 'N FERTILIZATION FILE: NO N FERTILIZATION DATA USED'
else
   print *, 'N FERTILIZATION FILE:  ',io%nfert_in
endif

if(io%soilwarm_in == '') then
   print *, 'SOIL WARMING FILE: NO SOIL WARMING DATA USED'
else
   print *, 'SOIL WARMING FILE:  ',io%nfert_in
endif
if(io%restart_in == '') then
    print *,'INITIAL CONDITIONS FILE: NO RESTART FILE LISTED USING DEFAULTS'
else
  print *, 'INITIAL CONDITIONS FILE:  ',io%restart_in
endif

if(io%day_out /= '') then
   open(5,file=io%day_out,status='REPLACE')
   close(5)

endif

open(5,file = io%annual_out,status='REPLACE')
close(5)

print *, 'SIMULATION LENGTH (YEARS):',sim_length
print *, 'SPIN-UP LENGTH (YEARS):',spinup_length
print *,'----------------------------'

end subroutine read_namelist

end module aconite_io
