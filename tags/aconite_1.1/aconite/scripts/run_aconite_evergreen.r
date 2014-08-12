
run_aconite <- function(file1,file2,plot_day,total_sim,start,end,annual_doy,spinup_length){



        fileConn <-file("aconite_namelist")

        CLIM_FILE='climate_HF_2002.csv'
        PARAM_FILE='parameter_temp_evergreen.txt'
        DAY_OUT=file1
        ANNUAL_OUT=file2
        RESTART_IN='restart_temp_evergreen.txt'
        RESTART_OUT='restart_test.txt'
	    DIST_IN='dist_HB.txt'
	    SITE_IN='site_details_HF.txt'
	    NFERT_IN=''
	    SOILWARM_IN=''
	    SIM_LENGTH=total_sim
	    CLIM_LENGTH=4015
	    DIST_LENGTH=2
	    NFERT_LENGTH = 0
	    SOILWARM_LENGTH= 6
	    YEAR_OUT_START = start
	    YEAR_OUT_END = end
        ANNUAL_DOY = annual_doy
        SIM_START_YEAR = 1800
        COLD_START = 1
        SPINUP_LENGTH=spinup_length
	    SPINUP_CLIM_LENGTH=365

        CLIM_STRING = paste("CLIM_FILE='",CLIM_FILE,"'",sep="")
        PARAM_STRING = paste("PARAM_FILE='",PARAM_FILE,"'",sep="")
        DAY_STRING = paste("DAY_OUT='",DAY_OUT,"'",sep="")       
        ANNUAL_STRING = paste("ANNUAL_OUT='",ANNUAL_OUT,"'",sep="")
        RESTART_IN_STRING = paste("RESTART_IN='",RESTART_IN,"'",sep="")
        RESTART_OUT_STRING = paste("RESTART_OUT='",RESTART_OUT,"'",sep="")
        SITE_STRING = paste("SITE_IN='",SITE_IN,"'",sep="")
        DIST_STRING = paste("DIST_IN='",DIST_IN,"'",sep="")
        NFERT_STRING = paste("NFERT_IN='",NFERT_IN,"'",sep="")
        SOILWARM_STRING = paste("SOILWARM_IN='",SOILWARM_IN,"'",sep="")
        SIM_LENGTH_STRING = paste("SIM_LENGTH=",SIM_LENGTH,sep="")
        CLIM_LENGTH_STRING = paste("CLIM_LENGTH=",CLIM_LENGTH,sep="")
        DIST_LENGTH_STRING = paste("DIST_LENGTH=",DIST_LENGTH,sep="")
        NFERT_LENGTH_STRING = paste("NFERT_LENGTH=",NFERT_LENGTH,sep="")
        SOILWARM_LENGTH_STRING = paste("SOILWARM_LENGTH=",SOILWARM_LENGTH,sep="")
        YEAR_OUT_START_LENGTH = paste("PRINT_YEAR_START=",YEAR_OUT_START,sep="") 
        YEAR_OUT_END_LENGTH = paste("PRINT_YEAR_END=",YEAR_OUT_END,sep="")    
        ANNUAL_STATE_DOY = paste("ANNUAL_STATE_DOY=",ANNUAL_DOY,sep="")      
        SIM_START_YEAR= paste("SIM_START_YEAR=",SIM_START_YEAR,sep="")      
        COLD_START = paste("COLD_START=",COLD_START,sep="")
        SPINUP_LENGTH = paste("SPINUP_LENGTH=",SPINUP_LENGTH,sep="")
        SPINUP_CLIM_LENGTH = paste("SPINUP_CLIM_LENGTH=",SPINUP_CLIM_LENGTH,sep="")        
        writeLines(c("&ACONITE_IN",CLIM_STRING,PARAM_STRING,DAY_STRING,ANNUAL_STRING,RESTART_IN_STRING,RESTART_OUT_STRING,SITE_STRING,DIST_STRING,NFERT_STRING,SOILWARM_STRING,SIM_LENGTH_STRING,CLIM_LENGTH_STRING,DIST_LENGTH_STRING,NFERT_LENGTH_STRING,SOILWARM_LENGTH_STRING,YEAR_OUT_START_LENGTH,YEAR_OUT_END_LENGTH,ANNUAL_STATE_DOY,SIM_START_YEAR,COLD_START,SPINUP_LENGTH,SPINUP_CLIM_LENGTH,"/"),fileConn)
        close(fileConn)
      
      
       
        system("./run_aconite")
        if(plot_day == 1){  
        	data1 = read.table(file1,header=TRUE)
        }else{
         	data1 = read.table(file2,header=TRUE)
		}
        
        attach(data1)
        
        TIME = MODEL_YEAR   
     
        pdf('ACONITE_PLOTS_decid.pdf')        
        temp_maxX = max(TIME)
        temp_minX = min(TIME)
        
        CUE = (NPP/GPP)
        CUE[which(!is.finite(CUE))]=NA
            
        plot(TIME ,LAI,xlab ='time',ylab='LAI',type='l')
        plot(TIME ,LEAF_C,xlab = 'time',ylab = 'LEAFC',type='l')
        plot(TIME ,WOOD_C,xlab ='time',ylab='WOODC',type='l')
        plot(TIME ,ROOT_C,xlab = 'time',ylab = 'ROOTC',type='l')
        plot(TIME,ROOT_C/LEAF_C,xlab = 'time',ylab = 'ROOTC/LEAFC RATIO',type='l')
        plot(TIME,LABILE_C,xlab ='time',ylab='LABILE C',type='l')
        plot(TIME,LABILE_C_BUD,xlab = 'time',ylab = 'LABILE C BUD',type='l')
        plot(TIME,LABILE_C_RA,xlab = 'time',ylab = 'LABILE C Ra',type='l')                 
        plot(TIME,MAX_C_STORE,xlab = 'time',ylab='Max C store',type = 'l')
        plot(TIME,(LABILE_C/MAX_C_STORE),xlab ='time',ylab='FRACTION OF MAX C STORE',type='l')
        abline(1,0)
        plot(TIME,LEAF_N,xlab = 'time',ylab = 'LEAF N',type='l')
        plot(TIME,WOOD_N,xlab ='time',ylab='WOOD N' ,type='l')
        plot(TIME,ROOT_N,xlab = 'time',ylab = 'ROOT N',type='l')
        plot(TIME,LABILE_N,xlab ='time',ylab='LABILE N' ,type='l')  
        plot(TIME,LABILE_N_BUD,xlab = 'time',ylab = 'LABILE N BUD',type='l')      
        plot(TIME,MAX_N_STORE,xlab = 'time',ylab='Max N store',type = 'l')
        plot(TIME,(LABILE_N/MAX_N_STORE),xlab ='time',ylab='FRACTION OF MAX N STORE' ,type='l')
        abline(1,0)
        plot(TIME,TOTVEGC,xlab = 'time',ylab = 'TOTVEGC',type='l') 
        plot(TIME,TOTVEGN,xlab ='time',ylab='TOTVEGN',type='l')
        plot(TIME,CWDC,xlab = 'time',ylab = 'CWDC',type='l')
        plot(TIME,CWDN,xlab = 'time',ylab = 'CWDN',type='l')
        plot(TIME,LITTER_C,xlab = 'time',ylab = 'LITTERC',type='l')
        plot(TIME,LITTER_N,xlab = 'time',ylab = 'LITTERN',type='l')
        plot(TIME,SOIL_C,xlab = 'time',ylab = 'SOILC',type='l')
        plot(TIME,SOIL_N,xlab = 'time',ylab = 'SOILN',type='l')
        plot(TIME,SOIL_NH4,xlab = 'time',ylab = 'NH4',type='l')
        plot(TIME,SOIL_NO3,xlab = 'time',ylab = 'NO3',type='l')
        plot(TIME,GPP,xlab = 'time',ylab = 'GPP',type='l')
        plot(TIME,NPP,xlab = 'time',ylab = 'NPP',type='l')
        plot(TIME,CUE,xlab = 'time',ylab = 'CUE (NPP/GPP)',type='l') 
        plot(TIME,RA,xlab = 'time',ylab = 'Ra',type='l')
        plot(TIME,RA_GROWTH,xlab = 'time',ylab = 'Ra Grow',type='l')
        plot(TIME,RA_MAINT,xlab = 'time',ylab = 'Ra main',type='l')
        plot(TIME,RA_EXCESS,xlab = 'time',ylab = 'Ra excessC',type='l')
        plot(TIME,RH,xlab = 'time',ylab = 'Rh',type='l')
        plot(TIME,NEE,xlab = 'time',ylab = 'NEE',type='l')
        plot(TIME,PLANT_NUPTAKE,xlab = 'time',ylab = 'NUPTAKE',type='l')
        plot(TIME,RETRANSLOCATED_N,xlab = 'time',ylab = 'RETRANSN',type='l')
        plot(TIME,N_IMMOBOLIZATION,xlab = 'time',ylab = 'IMMOBILIZATION',type='l')
        plot(TIME,NET_N_MIN,xlab = 'time',ylab = 'NET N MINERALIZATION',type='l')
        plot(TIME,NO3_LEACHING,xlab = 'time',ylab = 'NO3 LEACHING',type='l')
        
        temp_maxY = max(c(N_FIXATION,DON_LEACHING))
        temp_minY = min(c(N_FIXATION,DON_LEACHING))
        plot(TIME,N_FIXATION,ylim=c(temp_minY,temp_maxY),xlab = 'time',ylab = 'N INPUT/OUTPUT',type='l')
        points(TIME,DON_LEACHING,xlab = 'time',type='l',col='blue')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('N fixation','DON leaching'),lty=c(1,1),col=c('black','blue'))
        
        temp_maxY = max(c(N_DEPOSITION,NO3_LEACHING))
        temp_minY = min(c(N_DEPOSITION,NO3_LEACHING))
        plot(TIME,N_DEPOSITION,ylim=c(temp_minY,temp_maxY),xlab = 'time',ylab = 'N INPUT/OUTPUT',type='l')
        points(TIME,NO3_LEACHING,xlab = 'time',type='l',col='blue')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('N deposition','NO3 leaching'),lty=c(1,1),col=c('black','blue'))
  
        plot(TIME,NITRIFICATION,xlab = 'time',ylab = 'NITRIFICATION',type='l')
        plot(TIME,N_UPTAKE_DOWNREG,xlab = 'time',ylab = 'Nuptake downreg',type='l')
        plot(TIME,LEAF_CN,xlab = 'time',ylab = 'LEAF CN',type='l')
        plot(TIME,TARGET_LEAFCN,xlab = 'time',ylab = 'TARGET LEAF CN',type='l') 
        plot(TIME,MAX_LEAF_C,xlab = 'time',ylab = 'MAX LEAF C',type='l')               
        plot(TIME,MAX_ROOT_C,xlab = 'time',ylab = 'MAX ROOT C',type='l')
        plot(TIME,DEBUG,xlab = 'time',ylab = 'debug',type='l')
        plot(TIME,DEBUG_2,xlab = 'time',ylab = 'debug2',type='l')
        ################################
        temp_maxY = max(c(Creturn_leafC,Creturn_leafN,Creturn_leafCN))
        temp_minY = min(c(Creturn_leafC,Creturn_leafN,Creturn_leafCN))
		
        plot(TIME,Creturn_leafC,xlab = 'time',ylab = 'marginal return (g/g)',type='l',main ='Creturn leaf (integrated)',ylim = c(temp_minY,temp_maxY),lty=1,col='red')
        points(TIME,Creturn_leafN,type = 'l',lty=1,col='green')
        points(TIME,Creturn_leafCN,type = 'l',lty=1,col='blue')
        abline(0,0,lty=1,col='black')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('leafC','leafN','leafCN','additive'),lty=c(1,1,1,1),col=c('red','green','blue','gray'))
        ##################################
        temp_maxY = max(c(Nreturn_rootN,Nreturn_Raexcess,Nreturn_rootCN,Nreturn_rootC))
        temp_minY = min(c(Nreturn_rootN,Nreturn_Raexcess,Nreturn_rootCN,Nreturn_rootC))
        plot(TIME,Nreturn_rootC,xlab = 'time',ylab = 'marginal return (g/g)',type='l',main = 'Nreturn root (integrated)',ylim = c(temp_minY,temp_maxY),lty=1,col='red')
        points(TIME,Nreturn_rootN,type = 'l',lty=1,col='green')
        points(TIME,Nreturn_rootCN,type = 'l',lty=1,col='blue')
        points(TIME,Nreturn_Raexcess,type = 'l',lty=1,col='orange')
        abline(0,0,lty=1,col='black')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('rootC','rootN','rootCN','C to Nfix','zero'),lty=c(1,1,1,1,1),col=c('red','green','blue','orange','black'))  
       
        temp_maxY = max(c(ALLOCATION_TO_LEAFC,TURNOVER_LEAF_C))
        temp_minY = min(c(ALLOCATION_TO_LEAFC,TURNOVER_LEAF_C))
        plot(TIME,ALLOCATION_TO_LEAFC,xlab = 'time',ylab = 'LEAF C ',type='l',ylim=c(temp_minY,temp_maxY),col='red',main='leafC alloc/turn')
        points(TIME,TURNOVER_LEAF_C,type='l',col='green')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('allocation','turnover'),lty=c(1,1),col=c('red','green'))
        ##############
        temp_maxY = max(c(ALLOCATION_TO_WOODC,TURNOVER_WOOD_C))
        temp_minY = min(c(ALLOCATION_TO_WOODC,TURNOVER_WOOD_C))
        plot(TIME,ALLOCATION_TO_WOODC,xlab = 'time',ylab = 'WOOD C',type='l',ylim=c(temp_minY,temp_maxY),col='red',main='woodC alloc/turn')
        points(TIME,TURNOVER_WOOD_C,type='l',col='green')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('allocation','turnover'),lty=c(1,1),col=c('red','green'))
        ###############
        temp_maxY = max(c(ALLOCATION_TO_ROOTC,TURNOVER_ROOT_C))
        temp_minY = min(c(ALLOCATION_TO_ROOTC,TURNOVER_ROOT_C))
        plot(TIME,ALLOCATION_TO_ROOTC,xlab = 'time',ylab = 'ROOT C',type='l',ylim=c(temp_minY,temp_maxY),col='red',main='ROOTC alloc/turn')
        points(TIME,TURNOVER_ROOT_C,type='l',col='green')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('allocation','turnover'),lty=c(1,1),col=c('red','green'))
        ######
        temp_maxY = max(c(ALLOCATION_TO_LEAFN,TURNOVER_LEAF_N))
        temp_minY = min(c(ALLOCATION_TO_LEAFN,TURNOVER_LEAF_N))
        plot(TIME,ALLOCATION_TO_LEAFN,xlab = 'time',ylab = 'LEAF N',type='l',ylim=c(temp_minY,temp_maxY),col='red',main='LEAFN alloc/turn')
        points(TIME,TURNOVER_LEAF_N,type='l',col='green')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('allocation','turnover'),lty=c(1,1),col=c('red','green'))
        ############
        temp_maxY = max(c(ALLOCATION_TO_WOODN,TURNOVER_WOOD_N))
        temp_minY = min(c(ALLOCATION_TO_WOODN,TURNOVER_WOOD_N))
        plot(TIME,ALLOCATION_TO_WOODN,xlab = 'time',ylab = 'WOOD N',type='l',ylim=c(temp_minY,temp_maxY),col='red',main='WOODN alloc/turn')
        points(TIME,TURNOVER_WOOD_N,type='l',col='green')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('allocation','turnover'),lty=c(1,1),col=c('red','green'))
        ############
        temp_maxY = max(c(ALLOCATION_TO_ROOTN,TURNOVER_ROOT_N))
        temp_minY = min(c(ALLOCATION_TO_ROOTN,TURNOVER_ROOT_N))
        plot(TIME,ALLOCATION_TO_ROOTN,xlab = 'time',ylab = 'ROOT N',type='l',ylim=c(temp_minY,temp_maxY),col='red',main='ROOTN alloc/turn')
        points(TIME,TURNOVER_ROOT_N,type='l',col='green')
        legend(x=((temp_minX+temp_maxX)/2),y=((temp_minY+temp_maxY)/2),legend=c('allocation','turnover'),lty=c(1,1),col=c('red','green'))
        ###################
        dev.off()
       deattach(data1)
}
