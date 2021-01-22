library(R6)
library(dplyr)

# List of tables needed
# 1) ds_spec (dataset, label):
#  This contians each dataset in the study, with the labels for each
# 2) ds_vars (dataset, variable, keep, key, codelist, origin, derivation_id):
#  This has information on what variables are in each dataset + plus dataset specific variable information
# 3) var_spec (variable, label, length, ?format [might be the same as code list]):
#  This has variable information that is shared across all datasets
# 4) value_spec (dataset, variaable, where, type, codelist, origin, derivation_id):
#  This has parametere specific infromation, as data is long the specs for wbc might be difference the hgb
# 5) derivations (derivation_id, derivation):
#  This contains derivation, it allows for different variables to have the same derivation. (derivation just string)
# 6) codelist (codelist, code, decode):
#  This contains the code/decode information, [with the potential to have format as a decode]
# 7) change log

# Mock examples of the data from the xml
ds_spec <- tribble(
   ~dataset, ~label,
   "DM",     "Demographics",
   "AE",     "Adverse Events",
   "LB",     "Labratory Test Results",
   "EX",     "Exposure"

)


ds_vars <- tribble(
   ~dataset, ~variable, ~keep, ~key,
   "DM",     "STUDYID", TRUE,   1,
   "DM",     "DOMAIN",	TRUE,   NA,
   "DM",     "USUBJID",	TRUE,	  2,
   "DM", 	 "SUBJID",	TRUE,	  NA,
   "DM",     "RFSTDTC",	TRUE,	  NA,
   "DM", 	 "RFENDTC",	TRUE,   NA,
   "DM",     "RFXSTDTC",TRUE,   NA,
   "DM",     "RFXENDTC",TRUE,   NA,
   "DM",     "RFICDTC",	TRUE,   NA,
   "DM", 	 "RFPENDTC",TRUE,   NA,
   "DM",     "DTHDTC",	TRUE,   NA,
   "DM", 	 "DTHFL",	TRUE,   NA,
   "DM", 	 "SITEID",	TRUE,   NA,
   "DM",     "INVID",	TRUE,   NA,
   "DM",     "INVNAM",	TRUE,   NA,
   "DM",     "BRTHDTC",	TRUE,   NA,
   "DM",     "AGE",	   TRUE,   NA,
   "DM",     "AGEU",	   TRUE,   NA,
   "DM",	    "SEX",   	TRUE,   NA,
   "DM",	    "RACE",	   TRUE,   NA,
   "DM",     "ETHNIC",	TRUE,   NA,
   "DM",	    "ARMCD",	TRUE,   NA,
   "DM",     "ARM",	   TRUE,   NA,
   "DM", 	 "ACTARMCD",TRUE,   NA,
   "DM",     "ACTARM",	TRUE,   NA,
   "DM",     "COUNTRY",	TRUE,   NA,




)


var_spec <- tribble(
   ~variable, ~label,                               ~length,
   "STUDYID",	"Study Identifier",                     	6,
   "DOMAIN",	"Domain Abbreviation",	                  2,
   "USUBJID",	"Unique Subject Identifier",	            13,
   "SUBJID",	"Subject Identifier for the Study",	      8,
   "RFSTDTC",	"Subject Reference Start Date/Time",	   NA,
   "RFENDTC", "Subject Reference End Date/Time",      	NA,
   "RFXSTDTC",	"Date/Time of First Study Treatment",     NA	,
   "RFXENDTC",	"Date/Time of Last Study Treatment",	   NA,
   "RFICDTC",	"Date/Time of Informed Consent",          NA,
   "RFPENDTC",	"Date/Time of End of Participation",      NA	,
   "DTHDTC",	"Date/Time of Death",	                  NA,
   "DTHFL",	   "Subject Death Flag",	                  1,
   "SITEID",	"Study Site Identifier",	               6,
   "INVID",	   "Investigator Identifier",	               6,
   "INVNAM",	"Investigator Name",	                     23,
   "BRTHDTC",	"Date/Time of Birth",	                  NA,
   "AGE",	   "Age",	                                 8,
   "AGEU",	   "Age Units",	                           5,
   "SEX",	   "Sex",	                                 1,
   "RACE",	   "Race",	                                 41,
   "ETHNIC",	"Ethnicity",	                           22,
   "ARMCD",	   "Planned Arm Code",                    	8,
   "ARM",	   "Description of Planned Arm",	            73,
   "ACTARMCD",	"Actual Arm Code",	                     8,
   "ACTARM",	"Description of Actual Arm",	            73,
   "COUNTRY",	"Country",	                              3
)




value_spec <- tribble(
 ~dataset, ~variable,    ~where,   ~type,          ~codelist,                    ~origin,    ~derivation_id,
  "DM",   "STUDYID",       TRUE,    "text",            NULL,                      "Protocol",         NA,
  "DM",   "DOMAIN",        TRUE,    "text",            "SDTM Domain Abbreviation","Assigned",         NA,
  "DM",   "USUBJID",       TRUE,    "text",            NULL,                      "Assigned",  "usubj_comm",
  "DM",   "SUBJID",        TRUE,    "text",            NULL,                  "CRF Pages 61 137", 	 NA,
  "DM",   "RFSTDTC",       TRUE,    "date",            "iso", 	                  "Derived",    "rfstdc_der",
  "DM",   "RFENDTC",       TRUE,    "date",            "iso",	                  "Derived",	  "rfendtc_der",
  "DM",   "RFXSTDTC",      TRUE,    "date",            "iso",	                  "Derived",	  "rfstdc_der",
  "DM",   "RFXENDTC",      TRUE,    "date",            "iso",	                  "Derived",    "rfxendtc_der",
  "DM",   "RFICDTC",       TRUE,    "date",            "iso",	                  "Assigned",	  "NP",
  "DM",   "RFPENDTC",      TRUE,    "date",            "iso",	                  "Derived",	  "rfpendtc_der",
  "DM",   "DTHDTC",        TRUE,    "date",            "iso",	                "CRF Page 37",          NA,
  "DM",   "DTHFL",         TRUE,    "text",            "YONLY",	                  "Assigned",	       NA,
  "DM",   "SITEID",        TRUE,    "text",            NA,	                     "Assigned",	          NA,
  "DM",   "INVID",         TRUE,    "text",            NA,	                     "Assigned",	          NA,
  "DM",   "INVNAM",        TRUE,    "text",            NA,	                     "Assigned",	          NA,
  "DM",   "BRTHDTC",       TRUE,    "date",            "iso",	                "CRF Page 40",	       NA,
  "DM",   "AGE",           TRUE,    "integer",            NA,	                     "Derived",	        "age",
  "DM",   "AGEU",          TRUE,    "text",            "Age Unit",	               "Assigned",	       NA,
  "DM",   "SEX",           TRUE,    "text",            "Sex",	                "CRF Page 40",          NA,
  "DM",   "RACE",          TRUE,    "text",            "Race",	                  "Assigned",	       NA,
  "DM",   "ETHNIC",        TRUE,    "text",            "Ethnic Group",	       "CRF Page 40",          NA,
  "DM",   "ARMCD",	      TRUE,    "text",            "Arm Code",	              "Assigned",	       NA,
  "DM",   "ARM",           TRUE,    "text",            "Arm Cod",	              "Protocol",	       NA,
  "DM",   "ACTARMCD",      TRUE,    "text",            "Arm Code",	              "Assigned",	       NA,
  "DM",   "ACTARM",        TRUE,    "text",            "Arm Code",	              "Assigned",	       NA,
  "DM",   "COUNTRY",       TRUE,    "text",            "Country",	              "Assigned",	       NA,

)


derivations <- tribble(
   ~derivation_id, ~derivation,
   "usubj_comm", "Concatenation of STUDYID and SUBJID (with leading Zeros up to 6 chars)",
   "rfstdc_der", "MIN(EXSTDTC) GROUP BY SUBJID, in ISO8601",
   "rfendtc_der", "MAX(DSSTDTC) GROUP BY SUBJID, in ISO8601",
   "rfxendtc_der", "MAX(EXENDTC) GROUP BY SUBJID, in ISO8601",
   "NP", "Not Populated",
   "rfpendtc_der", "Max(Date/time when subject ended participation or follow-up in a trial, as defined in the protocol)",
   "age", "FLOOR((DM.RFSTDTC - DM.BRTHDTC)/365.25)"

)


code_list <- tribble(
   ~codelist, ~code,    ~decode,
   "ISO8601",   NA,      "ISO8601",
   "YONLY",    "Y",      "Yes",
   "Age Unit", "YEARS",  "YEARS",
   "Sex",      "F",      "Female",
   "Sex",      "M",      "Male",
   "Race",     "AMERICAN INDIAN OR ALASKA NATIVE", "AMERICAN INDIAN OR ALASKA NATIVE",
   "Race",     "ASIAN",  "ASIAN",
   "Race",     "BLACK OR AFRICAN AMERICAN",   "BLACK OR AFRICAN AMERICAN",
   "Race",     "MULTIPLE","MULTIPLE",
   "Race",     "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
   "Race",     "WHITE",  "WHITE",
   "Ethnic Group", "HISPANIC OR LATINO", "HISPANIC OR LATINO",
   "Ethnic Group", "NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO",
   "Arm Code", "A",          "CAB LA + RPV LA - PARALLEL",
   "Arm Code", "B",          "ABC/DTG/3TC - PARALLEL",
   "Arm Code", "C",          "Induction Phase participation only, not randomized into Maintenance Phase",
   "Arm Code", "NOTASSGN",   "Not Assigned",
   "Arm Code", "SCRNFAIL",   "Screen Failure",
   "Country",	"CAN",	"Canada",
   "Country",	"DEU",	"Germany",
   "Country",	"ESP",	"Spain",
   "Country",	"FRA",	"France",
   "Country",	"GBR",	"United Kingdom",
   "Country",	"ITA",	"Italy",
   "Country",	"JPN",	"Japan",
   "Country",	"NLD",	"Netherlands",
   "Country",	"RUS",	"Russian Federation",
   "Country",	"USA",	"United States",
   "Country",	"ZAF",	"South Africa"

)


DataDef <- R6Class("DataDef",
                   public = list(
                      initialize = function(ds_spec, ds_vars, var_spec,
                                            value_spec, derivations, code_list){
                         private$ds_spec <- ds_spec
                         private$ds_vars <- ds_vars
                         private$vars_spec <- vars_spec
                         private$value_spec <- param_specs
                         private$derivations <- derivations
                         private$code_list <- code_list
                      },
                      print = function(...){
                         cat(private$ds_spec %>% as.character() %>% paste0(collapse = "\n"))
                      }
                   ),
                   private = list(
                      ds_spec = NULL,
                      ds_vars = NULL,
                      vars_spec = NULL,
                      value_spec = NULL,
                      derivations = NULL,
                      code_list = NULL,
                      change_log = NULL
                   )
)

test <- DataDef$new(ds_spec, ds_vars, vars_specs,
            param_specs, derivations, code_list)

test
# Notes from creation, derivations are sometimes duplicated, should the builder reduce the duplicates


# Potential contravertial things I have done:
#   * Collapse format information to live in codelist table
#   * Movae origin, codelist and derivation id to value_spec from var_spec
