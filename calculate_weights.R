######################################
# CALCULATE UTILITY WEIGHTS
######################################
# calculate utility weights from all patients for which english is available - regardless of treatment date
source("qalyweights.R")

qaly <- data %>% dplyr::select(pat_id,redcap_event_name,vst_dt,visit_date,visit_month,visit_year,mobility=spe_pp_mobility,selfcare=spe_pp_selfcare,activity=spe_pp_activities,pain=spe_pp_pain,anxiety=spe_pp_anxiety,healthtoday=spe_pp_health,outc_tx_decision,outc_eligible,cured) %>%
  mutate(
    mobility = ifelse(mobility > 5 | is.na(mobility), NA, mobility),
    selfcare = ifelse(selfcare > 5 | is.na(selfcare), NA, selfcare),
    activity = ifelse(activity > 5 | is.na(activity), NA, activity),
    pain = ifelse(pain > 5 | is.na(pain), NA, pain),
    anxiety = ifelse(anxiety > 5 | is.na(anxiety), NA, anxiety)
  ) %>% unite(combined,mobility,selfcare,activity,pain,anxiety,sep="",remove=FALSE) %>%
  mutate(combined = replace(combined, combined=="NANANANANA", NA),
         language = ifelse(visit_date <"2017-05-24","English","Khmer"))

qalyweighted <- QALYweights(qaly)

qalyinitial_dist <- qalyweighted %>% filter(language=="Khmer" & redcap_event_name=="initial_arm_1") %>% left_join(fstage,by="pat_id")

qalyinitial <- qalyweighted %>% filter(language=="Khmer" & redcap_event_name=="initial_arm_1") %>% left_join(fstage,by="pat_id") %>% group_by(stage) %>% summarise(thai_utility_mean=mean(thai_utility,na.rm=T),thai_utility_median=median(thai_utility,na.rm=T),indo_utility_mean=mean(indo_utility,na.rm=T),indo_utility_median=median(indo_utility,na.rm=T),n=n(),indo_low=quantile(indo_utility,probs=0.025,na.rm=T),indo_high=quantile(indo_utility,probs=0.975,na.rm=T)) %>% filter(!is.na(stage))

qalyinitialcirr <- qalyweighted %>% filter(language=="Khmer" & redcap_event_name=="initial_arm_1") %>% left_join(fstage,by="pat_id") %>% mutate(cirr=stage,time="infected")
qalyinitialcirr$cirr[qalyinitialcirr$cirr %in% c("F0","F1","F2","F3")] <- "pre-cirrhosis"
qalyinitialcirrgroup <- qalyinitialcirr %>% group_by(cirr,time) %>% summarise(thai_utility_mean=mean(thai_utility,na.rm=T),thai_utility_median=median(thai_utility,na.rm=T),indo_utility_mean=mean(indo_utility,na.rm=T),indo_utility_median=median(indo_utility,na.rm=T),n=n(),indo_low=quantile(indo_utility,probs=0.025,na.rm=T),indo_high=quantile(indo_utility,probs=0.975,na.rm=T))

# take mean of individuals first - change to use area under curve
# qalytreatment <- qalyweighted %>% filter(language=="Khmer" & outc_tx_decision %in% c("C","W","M","TF")) %>% group_by(pat_id) %>% summarise(thai_utility=mean(thai_utility,na.rm=T),indo_utility=mean(indo_utility,na.rm=T),n=n()) 
qalytreatment <- qalyweighted %>% filter(language=="Khmer" & outc_tx_decision %in% c("C","W","M","TF") & !is.na(indo_utility)) %>% group_by(pat_id) %>% arrange(pat_id,visit_date) %>% mutate(date = visit_date-first(visit_date),last_date=as.numeric(last(visit_date)-first(visit_date))) %>% summarise(thai_utility=ifelse(n()>1,trapz(date,thai_utility)/last_date,thai_utility),indo_utility=ifelse(n()>1,trapz(date,indo_utility)/last_date,indo_utility),n=n()) %>% left_join(fstage,by="pat_id") %>% filter(!is.na(stage))

qalytreatmentgroup <- qalytreatment %>% group_by(stage) %>% summarise(thai_utility_mean=mean(thai_utility,na.rm=T),thai_utility_median=median(thai_utility,na.rm=T),indo_utility_mean=mean(indo_utility,na.rm=T),indo_utility_median=median(indo_utility,na.rm=T),n=n(),indo_low=quantile(indo_utility,probs=0.025,na.rm=T),indo_high=quantile(indo_utility,probs=0.975,na.rm=T))

qalytreatmentcirr <- qalytreatment %>% mutate(cirr=stage,time="treatment")
qalytreatmentcirr$cirr[qalytreatmentcirr$cirr %in% c("F0","F1","F2","F3")] <- "pre-cirrhosis"
qalytreatmentcirrgroup <- qalytreatmentcirr %>% group_by(cirr,time) %>% summarise(thai_utility_mean=mean(thai_utility,na.rm=T),thai_utility_median=median(thai_utility,na.rm=T),indo_utility_mean=mean(indo_utility,na.rm=T),indo_utility_median=median(indo_utility,na.rm=T),n=n(),indo_low=quantile(indo_utility,probs=0.025,na.rm=T),indo_high=quantile(indo_utility,probs=0.975,na.rm=T))

# includes multiple follow ups for some patients - trapz if multiples as for treatment.
qalySVR_dist <- qalyweighted %>% filter(language=="Khmer" & outc_eligible=="FUP" & !is.na(indo_utility))  %>% group_by(pat_id) %>% arrange(pat_id,visit_date) %>% mutate(date = visit_date-first(visit_date),last_date=as.numeric(last(visit_date)-first(visit_date))) %>% summarise(thai_utility=ifelse(n()>1,trapz(date,thai_utility)/last_date,thai_utility),indo_utility=ifelse(n()>1,trapz(date,indo_utility)/last_date,indo_utility),n=n()) %>% left_join(fstage,by="pat_id") %>% filter(!is.na(stage))

qalySVR <- qalySVR_dist %>% group_by(pat_id) %>% group_by(stage) %>% summarise(thai_utility_mean=mean(thai_utility,na.rm=T),thai_utility_median=median(thai_utility,na.rm=T),indo_utility_mean=mean(indo_utility,na.rm=T),indo_utility_median=median(indo_utility,na.rm=T),n=n(),indo_low=quantile(indo_utility,probs=0.025,na.rm=T),indo_high=quantile(indo_utility,probs=0.975,na.rm=T))

qalySVRcirr <- qalyweighted %>% filter(language=="Khmer" & outc_eligible=="FUP"  & cured==TRUE) %>% left_join(fstage,by="pat_id") %>% mutate(cirr=stage,time="SVR")

qalySVRcirr$cirr[qalySVRcirr$cirr %in% c("F0","F1","F2","F3")] <- "pre-cirrhosis"

qalySVRcirrgroup <- qalySVRcirr %>% group_by(cirr,time) %>% summarise(thai_utility_mean=mean(thai_utility,na.rm=T),thai_utility_median=median(thai_utility,na.rm=T),indo_utility_mean=mean(indo_utility,na.rm=T),indo_utility_median=median(indo_utility,na.rm=T),n=n(),indo_low=quantile(indo_utility,probs=0.025,na.rm=T),indo_high=quantile(indo_utility,probs=0.975,na.rm=T))

qalycirr <- bind_rows(qalyinitialcirrgroup,qalytreatmentcirrgroup,qalySVRcirrgroup)