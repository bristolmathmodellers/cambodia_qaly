# calculate QALY weights

QALYweights <- function(qaly) {
  
    # Thailand value set
    lookup <- read.table("eq5dlookupthailand.txt",header=T)
    lookup <- lookup %>% unite(combined,mobility,selfcare,activity,pain,anxiety,sep="",remove=TRUE)
    
    qalyweights <- full_join(qaly,lookup,by="combined") %>% rename(thai_utility = EQ5Dindex)

    # Indonesia value set
    
    qalyweights<-qalyweights%>%
      mutate(
        indo_utility = 1,
        indo_utility = ifelse(mobility == "1",indo_utility - 0,
                              ifelse(mobility =="2",indo_utility - 0.119,
                                     ifelse(mobility =="3",indo_utility - 0.192,
                                            ifelse(mobility =="4",indo_utility - 0.410,
                                                   ifelse(mobility =="5",indo_utility - 0.613,
                                                          9))))),
        indo_utility = ifelse(selfcare == "1",indo_utility - 0,
                              ifelse(selfcare =="2",indo_utility - 0.101,
                                     ifelse(selfcare =="3",indo_utility - 0.140,
                                            ifelse(selfcare =="4",indo_utility - 0.248,
                                                   ifelse(selfcare =="5",indo_utility - 0.316,
                                                          9))))),
        indo_utility = ifelse(activity == "1",indo_utility - 0,
                              ifelse(activity =="2",indo_utility - 0.090,
                                     ifelse(activity =="3",indo_utility - 0.156,
                                            ifelse(activity =="4",indo_utility - 0.301,
                                                   ifelse(activity =="5",indo_utility - 0.385,
                                                          9))))),
        indo_utility = ifelse(pain == "1",indo_utility - 0, 
                              ifelse(pain =="2",indo_utility - 0.086, 
                                     ifelse(pain =="3",indo_utility - 0.095,
                                            ifelse(pain =="4",indo_utility - 0.198,
                                                   ifelse(pain =="5",indo_utility - 0.246,
                                                          9))))),
        indo_utility = ifelse(anxiety == "1",indo_utility - 0, 
                              ifelse(anxiety =="2",indo_utility - 0.079,
                                     ifelse(anxiety =="3",indo_utility - 0.134,
                                            ifelse(anxiety =="4",indo_utility - 0.227,
                                                   ifelse(anxiety =="5",indo_utility - 0.305,
                                                          9)))))#,
        #indo_utility = ifelse(mobility>5 | selfcare>5| activity>5 | pain>5 | anxiety>5 ,9, indo_utility)
        
      ) 
    
    
  return(qalyweights)
}