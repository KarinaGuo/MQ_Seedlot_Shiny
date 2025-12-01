## Seedlot_plot.R for shiny - Dated:28-11-2025

## Creating spreadsheet
### Prev project
seedlings_all <- read.csv ("data/Mquin_samples_pheno.csv") # Meta data for seedlings including height and COI of all seedlings, including those that didn't get genotypes

mat_line_rustassay_score <- seedlings_all %>% dplyr::select(FID, COI) %>% group_by(FID) %>% mutate(Seedling_number_rustassay = n()) %>% unique() 
colnames(mat_line_rustassay_score) <- (c("Seedlot", "Seedling_score_rustassay", "Seedling_number_rustassay"))

mat_line_meta <- seedlings_all %>% dplyr::select(FID, Lat, Long) %>% group_by(FID) %>% unique() 
colnames(mat_line_meta) <- (c("Seedlot", "Latitude", "Longitude"))

### Adults from Prev project
parent_meta_lib <- read.csv ("data/parent_meta.csv") %>% 
  dplyr::select(NSWID, sample_lib_NSW)
Mat_line_droughttol <- read.csv(file = "data/GAPIT49_prediction_gt.csv")
colnames(Mat_line_droughttol)[1] <- 'sample_lib_NSW'
Mat_line_droughttol <- left_join(Mat_line_droughttol, parent_meta_lib) %>% 
  dplyr::select (NSWID, Prediction)
Mat_line_droughttol_rmdup <- Mat_line_droughttol[!duplicated(Mat_line_droughttol$NSWID), ]
colnames(Mat_line_droughttol_rmdup) <- c("Seedlot", "MatLine_Genompred")

mat_line_meta <- left_join(mat_line_meta, Mat_line_droughttol_rmdup)

## Add in drought seedling predictions individuals 

sample_meta_drought <- readxl::read_xlsx("data/All_samples.xlsx") %>% filter(!is.na(NSWID) & `Genotyping Purpose`=="Drought_experiment") %>% dplyr::select(NSWID, Drought_Exp_ID)
colnames(sample_meta_drought)[2] <- c("IID"); sample_meta_drought$IID <- as.numeric(sample_meta_drought$IID)

prediction_gt <- read.csv("data/Report10753_prediction_gt_main.csv") %>% dplyr::select(c(Taxa, PEV, Prediction))
colnames(prediction_gt)[1] <- "NSWID"

prediction_gt_droughtexp <- inner_join(prediction_gt, sample_meta_drought, by="NSWID")
prediction_gt_droughtexp_rmpoorsamp <- prediction_gt_droughtexp %>% filter(PEV < 0.5)

IID_block <- readxl::read_xlsx(path="data/FID Selection & ID & Randomisation_vers1.xlsx", sheet = 'real_IID_Block_Cell_12tray') %>%  select(IID, Mat_Line)
IID_block <- left_join(IID_block, sample_meta_drought)

prediction_gt_droughtexp_rmpoorsamp <- left_join(IID_block, prediction_gt_droughtexp_rmpoorsamp) %>% 
  filter(!is.na(Prediction)) %>% 
  select(-c(NSWID))

# Add in GP from iter 48
iter_48_GP <- read.csv("data/GAPIT48_prediction_gt.csv"); colnames(iter_48_GP)[1] <- "LIBRARY"
prev_exp_pedi <- read.csv("data/prev_exp_pedi.csv")
prev_exp_pedi$FID <- gsub ("_", "", prev_exp_pedi$FID)

iter_48_GP_pedi <- left_join(iter_48_GP, prev_exp_pedi) %>% 
  select(LIBRARY, FID, Prediction, PEV)

colnames(prediction_gt_droughtexp_rmpoorsamp) <- c("LIBRARY", "FID", "Prediction", "PEV")

mat_line_genompred_score <- rbind(prediction_gt_droughtexp_rmpoorsamp, iter_48_GP_pedi) %>% 
  group_by(FID) %>% 
  mutate(Seedling_number_genompred = n()) %>% 
  select(FID, Prediction, Seedling_number_genompred)

colnames(mat_line_genompred_score) <- c("Seedlot", "Seedling_score_genompred", "Seedling_number_genompred")

mat_line_genompred_score$Seedling_score_genompred[mat_line_genompred_score$Seedling_score_genompred<0] = 0

## Final plot datasets
mat_line_rustassay_score <- left_join(mat_line_meta, mat_line_rustassay_score)
mat_line_genompred_score <- left_join(mat_line_meta, mat_line_genompred_score)

final_ds <- c("mat_line_rustassay_score", "mat_line_genompred_score")

rm(list=setdiff(ls(), final_ds))
