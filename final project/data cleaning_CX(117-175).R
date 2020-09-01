nis_clean <- read.csv("nis_clean.csv")
nis_clean <- nis_clean[,-1]
changing <- c("APRDRG_Risk_Mortality", "APRDRG_Severity", "CM_AIDS", "CM_ALCOHOL", "CM_ANEMDEF", "CM_ARTH", "CM_BLDLOSS CM_CHF", "CM_CHRNLUNG", "CM_COAG", "CM_DEPRESS", "CM_DM", "CM_DMCX", "CM_DRUG", "CM_HTN_C", "CM_HYPOTHY", "CM_LIVER", "CM_LYMPH", "CM_LYTES", "CM_METS", "CM_NEURO", "CM_OBESE", "CM_DRUG", "CM_HTN_C", "CM_HYPOTHY", "CM_LIVER", "CM_LYMPH", "CM_LYTES", "CM_METS", "CM_NEURO", "CM_PARA", "CM_PERIVASC", "CM_PSYCH", "CM_PULMCIRC", "CM_RENLFAIL", "CM_TUMOR", "CM_ULCER", "CM_VALVE", "CM_WGHTLOSS", "AWEEKEND", "DIED", "AMONTH", "DISPUNIFORM", "DQTR", "DRGVER", "HCUP_ED", "HOSPBRTH", "HOSP_DIVISION", "MDC", "MDC24", "MDC_NoPOA", "TRAN_IN", "TRAN_OUT", "RACE", "PL_NCHS2006", "ORPROC", "NEOMAT", "NIS_STRATUM", "CM_CHF", "CM_BLDLOSS")
for (i in changing) {
  nis_clean[,colnames(nis_clean) == i] <- as.factor(nis_clean[,colnames(nis_clean) == i])
}
summary(nis_clean)