**Hospital level variation in the development of PerCI***
**Create an avg VA risk score for each hospital 
gen adm_30= new_admitdate2 +30
format adm_30 %td
gen mort_30 = 1 if dod_09212018_pull <=adm_30
replace mort_30 = 0 if mort_30==.
tab mort_30

melogit mort_30 wbc_sc_rs albval_sc_rs bili_sc_rs bun_sc_rs glucose_sc_rs hct_sc_rs na_sc_rs pao2_sc_rs ph_sc_rs creat_sc_rs i.elx_grp_1_rs i.elx_grp_3_rs i.elx_grp_4_rs i.elx_grp_5_rs i.elx_grp_6_rs i.elx_grp_7_rs i.elx_grp_8_rs i.elx_grp_9_rs i.elx_grp_10_rs i.elx_grp_11_rs i.elx_grp_12_rs i.elx_grp_13_rs i.elx_grp_14_rs i.elx_grp_15_rs i.elx_grp_16_rs i.elx_grp_17_rs i.elx_grp_18_rs i.elx_grp_19_rs i.elx_grp_20_rs i.elx_grp_21_rs i.elx_grp_22_rs i.elx_grp_23_rs i.elx_grp_24_rs i.elx_grp_25_rs i.elx_grp_26_rs i.elx_grp_27_rs i.elx_grp_28_rs i.elx_grp_29_rs i.elx_grp_30_rs i.elx_grp_31_rs i.chf_nonhp_rs i.sepsis_rs i.alcohol_rs i.dysrhythmia_rs i.pneumonia_rs i.copd_rs i.coron_athero_rs i.osteoarthros_rs i.skin_infection_rs i.chestpain_rs i.complic_devi_rs i.uti_rs i.diabmel_w_cm_rs i.complic_proc_rs i.acute_ren_fail_rs i.backproblem_rs i.acute_mi_rs i.adlt_resp_fl_rs i.gi_hemorrhag_rs i.fluid_elc_dx_rs i.infect_parasitic_dis_rs i.neoplasms_rs i.endocrine_rs i.blood_dis_rs i.mental_illness_rs i.nervous_dis_rs i.circulatory_rs i.respiratory_rs i.digestive_rs i.genitourinary_rs i.pregnancy_rs i.skin_rs i.skin_rs i.musculoskeletal_rs i.congenital_anomalies_rs i.perinatal_rs i.injury_poisoning_rs i.illdefined_conditions_rs i.unclassified_rs i.hispanic_rs i.elx_grp_2_rs || facility:
estat icc
predict rintercept_adj, reffects reses(rintse_adj)
gen hosp_adj_mort30d=invlogit(_b[_cons] + rintercept_adj)
gen evenratelowerCI_adj=invlogit(_b[_cons] + rintercept_adj -1.96*sqrt(rintse_adj^2))
gen evenrateupperCI_adj=invlogit(_b[_cons] + rintercept_adj +1.96*sqrt(rintse_adj^2))
egen justone_mort30=tag(facility)
sort hosp_adj_mort30d
gen rank_hosp30d=sum(justone_mort30)
twoway (rbar evenratelowerCI_adj evenrateupperCI_adj rank_hosp30d if justone_mort30==1, barwidth(.1) fcolor(gold)) if justone_mort30==1|| dot hosp_adj_mort30d rank_hosp30d 

xtile hosp_adj_mort30d_quint = hosp_adj_mort30d,nq(5)
gen mort_30_20p= hosp_adj_mort30d if hosp_adj_mort30d_quint==1
sum mort_30_20p, d
gen mort_30_40p=hosp_adj_mort30d if hosp_adj_mort30d_quint==2
sum mort_30_40p, d
gen mort_30_60p=hosp_adj_mort30d if hosp_adj_mort30d_quint==3
sum mort_30_60p, d
gen mort_30_80p=hosp_adj_mort30d if hosp_adj_mort30d_quint==4
sum mort_30_80p, d
gen mort_30_100p=hosp_adj_mort30d if hosp_adj_mort30d_quint==5
sum mort_30_100p, d

**PerCI at ICU day 11
gen perci_11= perci
melogit perci_11 || facility:,or
estat icc
predict rintercept_adj_1, reffects reses(rintse_adj_1)
gen perci_11_rate=invlogit(_b[_cons] + rintercept_adj_1)
gen perci_11evenratelowerCI_adj=invlogit(_b[_cons] + rintercept_adj_1 -1.96*sqrt(rintse_adj_1^2))
gen perci_11evenrateupperCI_adj=invlogit(_b[_cons] + rintercept_adj_1 +1.96*sqrt(rintse_adj_1^2))
egen justone_perci11 =tag(perci_11_rate)
sort perci_11_rate
gen rank_perci11=sum(justone_perci11)
twoway (rbar perci_11evenratelowerCI_adj perci_11evenrateupperCI_adj rank_perci11 if justone_perci11==1, barwidth(.1) fcolor(gold)) if justone_perci11==1|| dot perci_11_rate rank_perci11


melogit perci_11 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn ||facility:
estat icc

melogit perci_11 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn i.new_teaching i.complex ||facility:               
estat icc

melogit perci_11 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn i.new_teaching i.complex hosp_adj_mort30d||facility:,or    
estat icc

margins, at(hosp_adj_mort30d = median(mort_30_20p)) at(hosp_adj_mort30d = median(mort_30_40p)) at(hosp_adj_mort30d = median(mort_30_60p)) at(hosp_adj_mort30d = median(mort_30_80p)) at(hosp_adj_mort30d = median(mort_30_100p))
marginsplot

**MOR
gen mor_adj=exp((sqrt(2*(var(_cons) )))*.6745)
gen morl_adj=exp((sqrt(2* var(_cons 95%CI)))*.6745)
gen moru_adj=exp((sqrt(2*((_cons 95%CI)))*.6745)


**Sensitivity analysis
**PERCI at 6 daysin the ICU
gen perci_6 = .
replace perci_6 = 1 if new_sum_icu_days_bedsection >=6 
replace perci_6 = 0 if new_sum_icu_days_bedsection <6

melogit perci_6 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn ||facility:
estat icc
predict rintercept_adj_2, reffects reses(rintse_adj_2)
gen perci_6_rate=invlogit(_b[_cons] + rintercept_adj_2)
gen perci_6evenratelowerCI_adj=invlogit(_b[_cons] + rintercept_adj_2 -1.96*sqrt(rintse_adj_2^2))
gen perci_6evenrateupperCI_adj=invlogit(_b[_cons] + rintercept_adj_2 +1.96*sqrt(rintse_adj_2^2))
egen justone_perci6 =tag(perci_6_rate)
sort perci_6_rate
gen rank_perci6=sum(justone_perci6)
twoway (rbar perci_6evenratelowerCI_adj perci_6evenrateupperCI_adj rank_perci6 if justone_perci6==1, barwidth(.1) fcolor(gold)) if justone_perci6==1|| dot perci_6_rate rank_perci6


melogit perci_6 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn i.new_teaching i.complex ||facility:               
estat icc

melogit perci_6 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn i.new_teaching i.complex hosp_adj_mort30d||facility:,or    
estat icc

*MOR
gen mor_6_adj=exp((sqrt(2*( var(_cons))))*.6745)
gen morl_6_adj=exp((sqrt(2*( var(_cons 95%CI))))*.6745)
gen moru_6_adj=exp((sqrt(2 *(var(_cons 95%CI))))*.6745)



**Perci at 16 days
gen perci_16=.
replace perci_16 = 1 if new_sum_icu_days_bedsection >=16 
replace perci_16 = 0 if new_sum_icu_days_bedsection <16

melogit perci_16 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn ||facility:
estat icc
predict rintercept_adj_3, reffects reses(rintse_adj_3)
gen perci_16_rate=invlogit(_b[_cons] + rintercept_adj_3)
gen perci_16evenratelowerCI_adj=invlogit(_b[_cons] + rintercept_adj_3 -1.96*sqrt(rintse_adj_3^2))
gen perci_16evenrateupperCI_adj=invlogit(_b[_cons] + rintercept_adj_3 +1.96*sqrt(rintse_adj_3^2))
egen justone_perci16 =tag(perci_16_rate)
sort perci_16_rate
gen rank_perci16=sum(justone_perci16)
twoway (rbar perci_16evenratelowerCI_adj perci_16evenrateupperCI_adj rank_perci16 if justone_perci16==1, barwidth(.1) fcolor(gold)) if justone_perci16==1|| dot perci_16_rate rank_perci16

melogit perci_16 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn i.new_teaching i.complex ||facility:               
estat icc

melogit perci_16 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn i.new_teaching i.complex hosp_adj_mort30d||facility:,or    
estat icc

*MOR
gen mor_16_adj=exp((sqrt(2*( var(_cons))))*.6745)
gen morl_16_adj=exp((sqrt(2*( var(_cons 95%CI))))*.6745)
gen moru_16_adj=exp((sqrt(2*( var(_cons 95%CI))))*.6745)

**ADDITIONAL SENSITIVTY ANALYSIS USING ICU MORTALITY INSTEAD OF 30 day hospital
gen icu_mort = 1 if dod_09212018_pull <= new_specialtydischargedate
replace icu_mort = 0 if icu_mort==.
tab icu_mort, m


melogit icu_mort wbc_sc_rs albval_sc_rs bili_sc_rs bun_sc_rs glucose_sc_rs hct_sc_rs na_sc_rs pao2_sc_rs ph_sc_rs creat_sc_rs i.elx_grp_1_rs i.elx_grp_3_rs i.elx_grp_4_rs i.elx_grp_5_rs i.elx_grp_6_rs i.elx_grp_7_rs i.elx_grp_8_rs i.elx_grp_9_rs i.elx_grp_10_rs i.elx_grp_11_rs i.elx_grp_12_rs i.elx_grp_13_rs i.elx_grp_14_rs i.elx_grp_15_rs i.elx_grp_16_rs i.elx_grp_17_rs i.elx_grp_18_rs i.elx_grp_19_rs i.elx_grp_20_rs i.elx_grp_21_rs i.elx_grp_22_rs i.elx_grp_23_rs i.elx_grp_24_rs i.elx_grp_25_rs i.elx_grp_26_rs i.elx_grp_27_rs i.elx_grp_28_rs i.elx_grp_29_rs i.elx_grp_30_rs i.elx_grp_31_rs i.chf_nonhp_rs i.sepsis_rs i.alcohol_rs i.dysrhythmia_rs i.pneumonia_rs i.copd_rs i.coron_athero_rs i.osteoarthros_rs i.skin_infection_rs i.chestpain_rs i.complic_devi_rs i.uti_rs i.diabmel_w_cm_rs i.complic_proc_rs i.acute_ren_fail_rs i.backproblem_rs i.acute_mi_rs i.adlt_resp_fl_rs i.gi_hemorrhag_rs i.fluid_elc_dx_rs i.infect_parasitic_dis_rs i.neoplasms_rs i.endocrine_rs i.blood_dis_rs i.mental_illness_rs i.nervous_dis_rs i.circulatory_rs i.respiratory_rs i.digestive_rs i.genitourinary_rs i.pregnancy_rs i.skin_rs i.skin_rs i.musculoskeletal_rs i.congenital_anomalies_rs i.perinatal_rs i.injury_poisoning_rs i.illdefined_conditions_rs i.unclassified_rs i.hispanic_rs i.elx_grp_2_rs || facility:
estat icc
predict rintercept_adj_4, reffects reses(rintse_adj_4)
gen icu_mort_rate=invlogit(_b[_cons] + rintercept_adj_4)
gen icu_mort_evenratelowerCI_adj=invlogit(_b[_cons] + rintercept_adj_4 -1.96*sqrt(rintse_adj_4^2))
gen icu_mort_evenrateupperCI_adj=invlogit(_b[_cons] + rintercept_adj_4 +1.96*sqrt(rintse_adj_4^2))
egen justone_icu_adjmort=tag(facility)
sort icu_mort_rate
gen rank_icu_mort=sum(justone_icu_adjmort)
twoway (rbar icu_mort_evenratelowerCI_adj icu_mort_evenrateupperCI_adj rank_icu_mort if justone_icu_adjmort==1, barwidth(.1) fcolor(gold)) if justone_icu_adjmort==1|| dot icu_mort_rate rank_icu_mort
sum icu_mort_rate, d

melogit perci_11 age ib6.Race ib2.Gender elixhauser_vanwalraven va_risk i.septicemia i.coron_athero i.hrt_valve_dx i.adlt_resp_fl i.acute_mi i.chf_nonhp i.brnch_lng_ca i.aneurysm i.pneumonia i.complic_devi i.dysrhythmia i.perip_athero i.alcohol_related_dis i.gi_hemorrhag i.complic_proc i.copd i.colon_cancer i.hd_nck_cancer i.diabmel_w_cm i.htn_complicn i.new_teaching i.complex icu_mort_rate||facility:,or    
estat icc
