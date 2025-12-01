## simulation_plots 

MR_iter <- c(0.3, 0.5, 0.9)
restoration <- c(TRUE, FALSE)
restoration_MR <- c(0.1 ,0.3 ,0.5)
restoration_numb <- c(50, 100, 300)

combinations_df <- expand.grid(
  MR_iter = MR_iter,
  restoration = restoration,
  restoration_MR = restoration_MR,
  restoration_numb = restoration_numb
)

paste0("ShinyAppRuns/SIZE_int_MR_", MR_death_impact, "_restoration_", intercept_togg, "_numb_",intercept_indiv, "_level_", intercept_MR_mean ,".csv"))


base_dir <- "~/Uni/Doctorate/Ch Natural selection/Simulation/ShinyAppRuns/"

for (MR in MR_iter){
  for (restoration_event in restoration){
    for (restoration_MR_res in restoration_MR) {
      for (restoration_num in restoration_numb) {
        filename = paste0(base_dir, "SIZE_int_MR_", MR, "_restoration_", restoration_event, "_numb_",restoration_num, "_level_", restoration_MR_res)
        cat(filename, "\n")
        #file <- read.csv (paste0(base_dir,"SIZE_int_MR_", MR, "_restoration_", restoration_event, "_numb_",restoration_num, "_level_", restoration_MR_res ,".csv"))
        
        #assign(filename, file)
      }
    }
  }
}

## Base plots
set_theme(theme_void)

base_plot_high_MR <- ggplot() +
  stat_smooth(data = SIZE_int_MR_0.9_restoration_FALSE_numb_50_level_0.5, aes(x=time, y=sum_size), linewidth = 0.85, colour="grey", span=10) +
  stat_smooth(data = SIZE_int_MR_0.9_restoration_TRUE_numb_100_level_0.5, aes(x=time, y=sum_size), linewidth = 0.85, colour="firebrick", span=10) +
  stat_smooth(data = SIZE_int_MR_0.9_restoration_TRUE_numb_300_level_0.5, aes(x=time, y=sum_size), linewidth = 0.85, colour="firebrick3", span=10) +
  stat_smooth(data = SIZE_int_MR_0.9_restoration_TRUE_numb_50_level_0.3, aes(x=time, y=sum_size), linewidth = 0.85, colour="orange2", span=10) +
  stat_smooth(data = SIZE_int_MR_0.9_restoration_TRUE_numb_100_level_0.3, aes(x=time, y=sum_size), linewidth = 0.85, colour="orange1", span=10) +
  stat_smooth(data = SIZE_int_MR_0.9_restoration_TRUE_numb_300_level_0.3, aes(x=time, y=sum_size), linewidth = 0.85, colour="goldenrod1", span=10) +
  stat_smooth(data = SIZE_int_MR_0.9_restoration_TRUE_numb_300_level_0.1, aes(x=time, y=sum_size), linewidth = 0.85, colour="chartreuse4", span=10) +
  stat_smooth(data = SIZE_int_MR_0.9_restoration_TRUE_numb_300_level_0.1, aes(x=time, y=sum_size), linewidth = 0.85, colour="chartreuse3", span=10) +
  stat_smooth(data = SIZE_int_MR_0.9_restoration_TRUE_numb_300_level_0.1, aes(x=time, y=sum_size), linewidth = 0.85, colour="chartreuse2", span=10) +
  geom_vline(xintercept=1000, linewidth = 0.75, linetype="dashed", colour="darkorchid4") +
  geom_vline(xintercept=1025, linewidth = 0.75, linetype="dashed", colour="orchid")

base_plot_mid_MR <- ggplot() +
  stat_smooth(data = SIZE_int_MR_0.5_restoration_FALSE_numb_50_level_0.5, aes(x=time, y=sum_size), linewidth = 0.85, colour="grey", span=10) +
  stat_smooth(data = SIZE_int_MR_0.5_restoration_TRUE_numb_100_level_0.5, aes(x=time, y=sum_size), linewidth = 0.85, colour="firebrick", span=10) +
  stat_smooth(data = SIZE_int_MR_0.5_restoration_TRUE_numb_300_level_0.5, aes(x=time, y=sum_size), linewidth = 0.85, colour="firebrick3", span=10) +
  stat_smooth(data = SIZE_int_MR_0.5_restoration_TRUE_numb_50_level_0.3, aes(x=time, y=sum_size), linewidth = 0.85, colour="orange2", span=10) +
  stat_smooth(data = SIZE_int_MR_0.5_restoration_TRUE_numb_100_level_0.3, aes(x=time, y=sum_size), linewidth = 0.85, colour="orange1", span=10) +
  stat_smooth(data = SIZE_int_MR_0.5_restoration_TRUE_numb_300_level_0.3, aes(x=time, y=sum_size), linewidth = 0.85, colour="goldenrod1", span=10) +
  stat_smooth(data = SIZE_int_MR_0.5_restoration_TRUE_numb_300_level_0.1, aes(x=time, y=sum_size), linewidth = 0.85, colour="chartreuse4", span=10) +
  stat_smooth(data = SIZE_int_MR_0.5_restoration_TRUE_numb_300_level_0.1, aes(x=time, y=sum_size), linewidth = 0.85, colour="chartreuse3", span=10) +
  stat_smooth(data = SIZE_int_MR_0.5_restoration_TRUE_numb_300_level_0.1, aes(x=time, y=sum_size), linewidth = 0.85, colour="chartreuse2", span=10) +
  geom_vline(xintercept=1000, linewidth = 0.75, linetype="dashed", colour="darkorchid4") +
  geom_vline(xintercept=1025, linewidth = 0.75, linetype="dashed", colour="orchid")

base_plot_low_MR <- ggplot() +
  stat_smooth(data = SIZE_int_MR_0.3_restoration_FALSE_numb_50_level_0.5, aes(x=time, y=sum_size), linewidth = 0.85, colour="grey", span=10) +
  stat_smooth(data = SIZE_int_MR_0.3_restoration_TRUE_numb_100_level_0.5, aes(x=time, y=sum_size), linewidth = 0.85, colour="firebrick", span=10) +
  stat_smooth(data = SIZE_int_MR_0.3_restoration_TRUE_numb_300_level_0.5, aes(x=time, y=sum_size), linewidth = 0.85, colour="firebrick3", span=10) +
  stat_smooth(data = SIZE_int_MR_0.3_restoration_TRUE_numb_50_level_0.3, aes(x=time, y=sum_size), linewidth = 0.85, colour="orange2", span=10) +
  stat_smooth(data = SIZE_int_MR_0.3_restoration_TRUE_numb_100_level_0.3, aes(x=time, y=sum_size), linewidth = 0.85, colour="orange1", span=10) +
  stat_smooth(data = SIZE_int_MR_0.3_restoration_TRUE_numb_300_level_0.3, aes(x=time, y=sum_size), linewidth = 0.85, colour="goldenrod1", span=10) +
  stat_smooth(data = SIZE_int_MR_0.3_restoration_TRUE_numb_300_level_0.1, aes(x=time, y=sum_size), linewidth = 0.85, colour="chartreuse4", span=10) +
  stat_smooth(data = SIZE_int_MR_0.3_restoration_TRUE_numb_300_level_0.1, aes(x=time, y=sum_size), linewidth = 0.85, colour="chartreuse3", span=10) +
  stat_smooth(data = SIZE_int_MR_0.3_restoration_TRUE_numb_300_level_0.1, aes(x=time, y=sum_size), linewidth = 0.85, colour="chartreuse2", span=10) +
  geom_vline(xintercept=1000, linewidth = 0.75, linetype="dashed", colour="darkorchid4") +
  geom_vline(xintercept=1025, linewidth = 0.75, linetype="dashed", colour="orchid")
  
ggsave(base_plot_low_MR, file=paste0(base_dir, "Low_MR_baseplot.jpg", width=2500, height=2000)
ggsave(base_plot_mid_MR, file=paste0(base_dir, "Mid_MR_baseplot.jpg", width=2500, height=2000)
ggsave(base_plot_high_MR, file=paste0(base_dir, "High_MR_baseplot.jpg", width=2500, height=2000)
       
## Tailored plots


