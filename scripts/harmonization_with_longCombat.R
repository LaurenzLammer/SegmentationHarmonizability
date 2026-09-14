# load relevant packages
library(longCombat)
# set working directory
# load the unadjusted data  
df <- read.csv(file = "life/Data/df_life.csv")
# load the WMHV data
df_wmhv <- read.csv(file = "life/Data/df_life_wmhv.csv")
# create a ghost col because longCombat would not work otherwise
df_wmhv$ghost_col <- 1
# load the uncorrected df
df_no_correction <- read.csv(file = "life/Data/df_life_no_correction.csv")

############### with random effect per id across batches ########################
# apply long combat
df_combat_interbatch <- longCombat(idvar='id', timevar='timepoint', formula='AGE + GENDER',
                        features = c("TGMV", "TCV", "LVV", "HCV",  "AV"),
                        ranef='(1|id)', batchvar = "SITE", data=df)
df_combat_interbatch <- df_combat_interbatch$data_combat

df_wmhv_combat_interbatch <- longCombat(idvar='id', timevar='timepoint', formula='AGE + GENDER',
                             features = c("WMHV_norm", "ghost_col"), ranef='(1|id)', batchvar = "SITE", data=df_wmhv)
df_wmhv_combat_interbatch <- df_wmhv_combat_interbatch$data_combat

# save dfs
write.csv(df_combat_interbatch, "life/Data/df_life_adj_interbatch.csv")
write.csv(df_wmhv_combat_interbatch, "life/Data/df_life_wmhv_adj_interbatch.csv")

############### with random effect per id only within batches ########################


# specify id per batch
df$id <- ifelse(grepl("samseg", df$SITE), paste0(df$id, "s"), paste0(df$id, "r"))
df_no_correction$id <- ifelse(grepl("samseg", df_no_correction$SITE), paste0(df_no_correction$id, "s"), paste0(df_no_correction$id, "r"))
df_wmhv$id <- ifelse(grepl("samseg", df_wmhv$SITE), paste0(df_wmhv$id, "s"), paste0(df_wmhv$id, "r"))

# apply long combat
df_combat <- longCombat(idvar='id', timevar='timepoint', formula='AGE + GENDER',
                        features = c("TGMV", "TCV", "LVV", "HCV",  "AV"),
                        ranef='(1|id)', batchvar = "SITE", data=df)
df_combat <- df_combat$data_combat

df_wmhv_combat <- longCombat(idvar='id', timevar='timepoint', formula='AGE + GENDER',
                             features = c("WMHV_norm", "ghost_col"), ranef='(1|id)', batchvar = "SITE", data=df_wmhv)
df_wmhv_combat <- df_wmhv_combat$data_combat

df_no_correction_combat <- longCombat(idvar='id', timevar='timepoint', formula='AGE + GENDER',
                                      features = c("TGMV", "TCV", "LVV", "HCV",  "AV"),
                                      ranef='(1|id)', batchvar = "SITE", data=df_no_correction)
df_no_correction_combat <- df_no_correction_combat$data_combat

# remove last letter from idvar
df_combat$id <- substr(df_combat$id, 1, nchar(df_combat$id) - 1)
df_no_correction_combat$id <- substr(df_no_correction_combat$id, 1, nchar(df_no_correction_combat$id) - 1)
df_wmhv_combat$id <- substr(df_wmhv_combat$id, 1, nchar(df_wmhv_combat$id) - 1)

# save dfs
write.csv(df_combat, "life/Data/df_life_adj.csv")
write.csv(df_wmhv_combat, "life/Data/df_life_wmhv_adj.csv")
write.csv(df_no_correction_combat, "life/Data/df_life_no_correction_adj.csv")

################# harmonize cross runs ######################
df_cross <- read.csv(file = "life/Data/df_life_cross.csv")
df_cross$id <- ifelse(grepl("samseg", df_cross$SITE), paste0(df_cross$id, "s"), paste0(df_cross$id, "r"))

# apply long combat
df_cross_combat <- longCombat(idvar='id', timevar='timepoint', formula='AGE + GENDER',
                              features = c("TGMV", "TCV", "LVV", "HCV",  "AV"),
                              ranef='(1|id)', batchvar = "SITE", data=df_cross)
df_cross_combat <- df_cross_combat$data_combat
df_cross_combat$id <- substr(df_cross_combat$id, 1, nchar(df_cross_combat$id) - 1)

# save df
write.csv(df_cross_combat, "life/Data/df_life_cross_adj.csv")

############### test long ComBat with its in-house functions ###################

# add missing covar cols
df_combat[,c("AGE", "GENDER")] <- df[,c("AGE", "GENDER")]
df_wmhv_combat[,c("AGE", "GENDER")] <- df_wmhv[,c("AGE", "GENDER")]

# test for additive effects
addTestTable <- addTest(idvar='id', 
                        batchvar='SITE',
                        features=c("TGMV", "TCV", "LVV", "HCV",  "AV"), 
                        formula='AGE + GENDER',
                        ranef='(1|id)',
                        data=df)

addTestTable_combat <- addTest(idvar='id', 
                               batchvar='SITE',
                               features=c("TGMV.combat", "TCV.combat", "LVV.combat", "HCV.combat",  "AV.combat"), 
                               formula='AGE + GENDER',
                               ranef='(1|id)',
                               data=df_combat)

addTestTable_wmhv <- addTest(idvar='id', 
                             batchvar='SITE',
                             features=c("WMHV_norm"), 
                             formula='AGE + GENDER',
                             ranef='(1|id)',
                             data=df_wmhv)

addTestTable_wmhv_combat <- addTest(idvar='id', 
                                    batchvar='SITE',
                                    features=c("WMHV_norm.combat"), 
                                    formula='AGE + GENDER',
                                    ranef='(1|id)',
                                    data=df_wmhv_combat)

# test for multiplicative effects
multTestTable <- multTest(idvar='id', 
                          batchvar='SITE',
                          features=c("TGMV", "TCV", "LVV", "HCV",  "AV"), 
                          formula='AGE + GENDER',
                          ranef='(1|id)',
                          data=df)

multTestTable_combat <- multTest(idvar='id', 
                                 batchvar='SITE',
                                 features=c("TGMV.combat", "TCV.combat", "LVV.combat", "HCV.combat",  "AV.combat"), 
                                 formula='AGE + GENDER',
                                 ranef='(1|id)',
                                 data=df_combat)

multTestTable_wmhv <- multTest(idvar='id', 
                               batchvar='SITE',
                               features=c("WMHV_norm"), 
                               formula='AGE + GENDER',
                               ranef='(1|id)',
                               data=df_wmhv)

multTestTable_wmhv_combat <- multTest(idvar='id', 
                                      batchvar='SITE',
                                      features=c("WMHV_norm.combat"), 
                                      formula='AGE + GENDER',
                                      ranef='(1|id)',
                                      data=df_wmhv_combat)

# check for data harmonized with neuroHarmonize
df_life_nh_bl <- read.csv(file = "life/Data/df_life_bl_adj.csv")
df_life_nh_fu <- read.csv(file = "life/Data/df_life_fu_adj.csv")
df_life_bl <- read.csv(file = "life/Data/df_life_bl.csv")
df_life_fu <- read.csv(file = "life/Data/df_life_fu.csv")

df_life_nh_bl[,c("id", "timepoint", "SITE", "AGE", "GENDER")] <- df_life_bl[,c("id", "timepoint", "SITE", "AGE", "GENDER")]
df_life_nh_fu[,c("id", "timepoint", "SITE", "AGE", "GENDER")] <- df_life_fu[,c("id", "timepoint", "SITE", "AGE", "GENDER")]

df_life_nh <- rbind(df_life_nh_bl, df_life_nh_fu)
colnames(df_life_nh) <- gsub("^X\\.\\.", "", colnames(df_life_nh))

df_life_wmhv_nh_bl <- read.csv(file = "life/Data/df_life_wmhv_bl_adj.csv")
df_life_wmhv_nh_fu <- read.csv(file = "life/Data/df_life_wmhv_fu_adj.csv")
df_life_wmhv_bl <- read.csv(file = "life/Data/df_life_wmhv_bl.csv")
df_life_wmhv_fu <- read.csv(file = "life/Data/df_life_wmhv_fu.csv")

df_life_wmhv_nh_bl[,c("id", "timepoint", "SITE", "AGE", "GENDER")] <- df_life_wmhv_bl[,c("id", "timepoint", "SITE", "AGE", "GENDER")]
df_life_wmhv_nh_fu[,c("id", "timepoint", "SITE", "AGE", "GENDER")] <- df_life_wmhv_fu[,c("id", "timepoint", "SITE", "AGE", "GENDER")]

df_life_wmhv_nh <- rbind(df_life_wmhv_nh_bl, df_life_wmhv_nh_fu)
colnames(df_life_wmhv_nh) <- gsub("^X\\.\\.", "", colnames(df_life_wmhv_nh))

addTestTable_nh <- addTest(idvar='id', 
                           batchvar='SITE',
                           features=c("TGMV", "TCV", "LVV", "HCV",  "AV"), 
                           formula='AGE + GENDER',
                           ranef='(1|id)',
                           data=df_life_nh)

multTestTable_nh <- multTest(idvar='id', 
                             batchvar='SITE',
                             features=c("TGMV", "TCV", "LVV", "HCV",  "AV"), 
                             formula='AGE + GENDER',
                             ranef='(1|id)',
                             data=df_life_nh)

addTestTable_wmhv_nh <- addTest(idvar='id', 
                                batchvar='SITE',
                                features=c("WMHV_norm"), 
                                formula='AGE + GENDER',
                                ranef='(1|id)',
                                data=df_life_wmhv_nh)

multTestTable_wmhv_nh <- multTest(idvar='id', 
                                  batchvar='SITE',
                                  features=c("WMHV_norm"), 
                                  formula='AGE + GENDER',
                                  ranef='(1|id)',
                                  data=df_life_wmhv_nh)

# save longcombat tests
longcombat_tests <- list(
  addTestTable = addTestTable,
  addTestTable_combat = addTestTable_combat,
  addTestTable_wmhv = addTestTable_wmhv,
  addTestTable_wmhv_combat = addTestTable_wmhv_combat,
  addTestTable_wmhv_nh = addTestTable_wmhv_nh,
  addTestTable_nh = addTestTable_nh,
  multTestTable = multTestTable,
  multTestTable_combat = multTestTable_combat,
  multTestTable_wmhv = multTestTable_wmhv,
  multTestTable_wmhv_combat = multTestTable_wmhv_combat,
  multTestTable_wmhv_nh = multTestTable_wmhv_nh,
  multTestTable_nh = multTestTable_nh
)

save(longcombat_tests, file = "Results/longcombat_tests.RData")
