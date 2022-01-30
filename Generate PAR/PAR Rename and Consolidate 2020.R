
setwd("C:/Users/cjcar/Dropbox/GeoMalaria/PARTables")

outdir = 'C:/Users/cjcar/Dropbox/GeoMalaria/PARTables-Renamed'

library(tidyverse)

for (scen in c("R8","GL")) {
  for (run in c(1:3)) {
    
    # Deal with stupid space issue
    if(run==2 && scen=='R8') {
      africa.s <- read_csv(paste(paste(paste('Africa_stable',scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'csv',sep='.'))
    } else {
      africa.s <- read_csv(paste(paste(paste('Africa_stable',scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'csv',sep=' .'))
    }    
    
    africa.u <- read_csv(paste(paste(paste('Africa_unstable',scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'csv',sep=' .'))
    
    write_csv(africa.s, 
              paste(outdir,paste(paste(paste("Pf_stable",scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'.csv',sep=''),sep='/'))
    write_csv(africa.u, 
              paste(outdir,paste(paste(paste("Pf_unstable",scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'.csv',sep=''),sep='/'))
    
    
    if(run==2 && scen=='R8') {
      asia.s <- read_csv(paste(paste(paste('Asia_stable',scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'csv',sep='.'))
    } else {
      asia.s <- read_csv(paste(paste(paste('Asia_stable',scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'csv',sep=' .'))
    }    
    
    asia.u <- read_csv(paste(paste(paste('Asia_unstable',scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'csv',sep=' .'))
    
    if(run==2 && scen=='R8') {
      lam.s <- read_csv(paste(paste(paste('LatAm_stable',scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'csv',sep='.'))
    } else {
      lam.s <- read_csv(paste(paste(paste('LatAm_stable',scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'csv',sep=' .'))
    }    
    
    lam.u <- read_csv(paste(paste(paste('LatAm_unstable',scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'csv',sep=' .'))
  
    viv.s <- left_join(asia.s, lam.s)
    viv.u <- left_join(asia.u, lam.u)
    
    write_csv(viv.s, 
              paste(outdir,paste(paste(paste("Pv_stable",scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'.csv',sep=''),sep='/'))
    write_csv(viv.u, 
              paste(outdir,paste(paste(paste("Pv_unstable",scen,sep="_"),paste(run,'2020',sep='_'),sep="_"),'.csv',sep=''),sep='/'))
    
  }
}

