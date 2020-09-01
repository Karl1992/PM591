nis_clean_new <- nis_clean[,c("AGE", "FEMALE", "RACE", "HOSPBRTH", "PL_NCHS2006", "ZIPINC_QRTL", "AMONTH", "AWEEKEND", "DQTR", "ELECTIVE", "LOS", "TRAN_IN", "TRAN_OUT", "CM_AIDS", "CM_ALCOHOL", "CM_ANEMDEF", "CM_ARTH", "CM_BLDLOSS",	"CM_CHF",	"CM_CHRNLUNG", "CM_COAG", "CM_DEPRESS", "CM_DM",	"CM_DMCX", "CM_DRUG", "CM_HTN_C",	"CM_HYPOTHY",	"CM_LIVER", "CM_LYMPH",	"CM_LYTES", "CM_METS", "CM_NEURO", "CM_OBESE", "CM_PARA", "CM_PERIVASC", "CM_PSYCH", "CM_PULMCIRC",	"CM_RENLFAIL", "CM_TUMOR", "CM_ULCER", "CM_VALVE",	"CM_WGHTLOSS", "NEOMAT", "DISPUNIFORM", "APRDRG_Risk_Mortality", "APRDRG_Severity", "MDC", "NDX", "ORPROC", "NCHRONIC", "NECODE", "HCUP_ED", "NPR", "HOSP_DIVISION", "NIS_STRATUM", "PAY1", "TOTCHG","DIED")]

HOSP_CONTROL <- numeric()
HOSP_LOCTEACH <- numeric()
HOSP_BEDSIZE <- numeric()
for (i in 1:length(nis_clean_new$NIS_STRATUM)) {
  inter <- strsplit(as.character(nis_clean_new$NIS_STRATUM[i]), split = "")[[1]]
  HOSP_CONTROL[i] <- as.numeric(inter[2])
  HOSP_LOCTEACH[i] <- as.numeric(inter[3])
  HOSP_BEDSIZE[i] <- as.numeric(inter[4])
}
nis_clean_new <- nis_clean_new[,-which(colnames(nis_clean_new) == "NIS_STRATUM")]
HOSP_CONTROL <- as.factor(HOSP_CONTROL)
HOSP_LOCTEACH <- as.factor(HOSP_LOCTEACH)
HOSP_BEDSIZE <- as.factor(HOSP_BEDSIZE)
nis_clean_new <- cbind(nis_clean_new, HOSP_CONTROL, HOSP_LOCTEACH, HOSP_BEDSIZE)