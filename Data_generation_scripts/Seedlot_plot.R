library(readxl)
## Creating spreadsheet
### Prev project
seedlings_all <- read.csv ("~/RBGSyd_Technical Officer/MQuin/Processing Meta/Mquin_samples_pheno.csv") # Meta data for seedlings including height and COI of all seedlings, including those that didn't get genotypes
JB_db <- seedlings_all %>%
  group_by(FID) %>%
  summarise(Seedlot = FID, 
            Seedling_number_rustassay = n(),
            Mean_seedling_score_rustassay = mean(COI, na.rm=T),
            Sd_seedling_score_rustassay = sd(COI, na.rm=T),
            latitude = Lat,
            longitude = Long) %>%
  unique() %>%
  ungroup() %>%
  select(-c(FID))

### Adilts from Prev project
parent_meta_lib <- read.csv ("~/Uni/Doctorate/Ch2 Stressor/parent_meta.csv") %>% 
  dplyr::select(NSWID, sample_lib_NSW)
Mat_line_droughttol <- read.csv(file = "C:/Users/swirl/OneDrive/Documents/RBGSyd_Technical Officer/MQuin/Seedling GWAS/Filtering/Iteration 6/data/GAPIT_49/prediction_gt.csv")
colnames(Mat_line_droughttol)[1] <- 'sample_lib_NSW'
Mat_line_droughttol <- left_join(Mat_line_droughttol, parent_meta_lib) %>% 
  dplyr::select (NSWID, Prediction)
Mat_line_droughttol_rmdup <- Mat_line_droughttol[!duplicated(Mat_line_droughttol$NSWID), ]
colnames(Mat_line_droughttol_rmdup) <- c("Seedlot", "MatLine_Genompred")

JB_db_MatlineGenompred <- left_join(JB_db, Mat_line_droughttol_rmdup)

## Add in drought seedling predictions individuals 

sample_meta_drought <- readxl::read_xlsx("C:/Users/swirl/OneDrive/Documents/Uni/Doctorate/Samples/All_samples.xlsx") %>% filter(!is.na(NSWID) & `Genotyping Purpose`=="Drought_experiment") %>% dplyr::select(NSWID, Drought_Exp_ID)
colnames(sample_meta_drought)[2] <- c("IID"); sample_meta_drought$IID <- as.numeric(sample_meta_drought$IID)

prediction_gt <- read.csv("~/Uni/Doctorate/Samples/Genotyping/Report-DMela25-10753/Report-DMela25-10753/GP_Out/prediction_gt_main.csv") %>% dplyr::select(c(Taxa, PEV, Prediction))
colnames(prediction_gt)[1] <- "NSWID"

prediction_gt_droughtexp <- inner_join(prediction_gt, sample_meta_drought, by="NSWID")
prediction_gt_droughtexp_rmpoorsamp <- prediction_gt_droughtexp %>% filter(PEV < 0.5)

IID_block <- read_xlsx(path="~/Uni/Doctorate/Ch2 Stressor/FID Selection & ID & Randomisation_vers1.xlsx", sheet = 'real_IID_Block_Cell_12tray') %>%  select(IID, Mat_Line)
IID_block <- left_join(IID_block, sample_meta_drought)

prediction_gt_droughtexp_rmpoorsamp <- left_join(IID_block, prediction_gt_droughtexp_rmpoorsamp) %>% 
  filter(!is.na(Prediction)) %>% 
  select(-c(NSWID))

# Add in GP from iter 48
iter_48_GP <- read.csv("~/RBGSyd_Technical Officer/MQuin/Seedling GWAS/Filtering/Iteration 6/data/GAPIT_48/prediction_gt.csv"); colnames(iter_48_GP)[1] <- "LIBRARY"
prev_exp_pedi <- read.csv("~/Uni/Doctorate/Samples/Seedlot_plot_data/data/prev_exp_pedi.csv")
prev_exp_pedi$FID <- gsub ("_", "", prev_exp_pedi$FID)

iter_48_GP_pedi <- left_join(iter_48_GP, prev_exp_pedi) %>% 
  select(LIBRARY, FID, Prediction, PEV)

colnames(prediction_gt_droughtexp_rmpoorsamp) <- c("LIBRARY", "FID", "Prediction", "PEV")

iter_48_GP_pedi_drought <- rbind(prediction_gt_droughtexp_rmpoorsamp, iter_48_GP_pedi)

iter_48_GP_pedi_summ <- iter_48_GP_pedi_drought %>% 
  group_by(FID) %>% 
  summarise(Seedling_number_genompred = n(),
            Mean_seedling_score_genompred = mean(Prediction, na.rm=T),
            Sd_seedling_score_genompred = sd(Prediction, na.rm=T))
colnames(iter_48_GP_pedi_summ)[1] <- "Seedlot"

JB_db_iter48 <- left_join(JB_db_MatlineGenompred, iter_48_GP_pedi_summ)  
JB_db_iter48$Mean_seedling_score_genompred[JB_db_iter48$Mean_seedling_score_genompred<0] = 0

write.csv(JB_db_iter48, "~/Uni/Doctorate/Samples/Seedlot_plot_data/final_seedloty_plot.csv", row.names=F)
