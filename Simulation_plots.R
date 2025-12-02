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

combinations_df$restoration_MR[!combinations_df$restoration] <- 0
combinations_df$restoration_numb[!combinations_df$restoration] <- 0
combinations_df <- unique(combinations_df)


#paste0("ShinyAppRuns/SIZE_int_MR_", MR_death_impact, "_restoration_", intercept_togg, "_numb_",intercept_indiv, "_level_", intercept_MR_mean ,".csv"))


base_dir <- "~/Uni/Doctorate/Ch Natural selection/Simulation/ShinyAppRuns/"

for (MR in MR_iter){
    for (restoration_MR_res in restoration_MR) {
      for (restoration_num in restoration_numb) {
        filename = paste0("SIZE_int_MR_", MR, "_restoration_", TRUE, "_numb_",restoration_num, "_level_", restoration_MR_res)
        #cat(filename, "\n")
        file <- read.csv (paste0(base_dir,"SIZE_int_MR_", MR, "_restoration_", TRUE, "_numb_",restoration_num, "_level_", restoration_MR_res ,".csv"))
        file <- file %>% mutate(MR=MR, restoration=TRUE, restoration_num=restoration_num, restoration_MR_res=restoration_MR_res)
        
        assign(filename, file)
    }
    }
  
  filename = paste0("SIZE_int_MR_", MR, "_restoration_FALSE_numb_0_level_0")
  file <- read.csv (paste0(base_dir,"SIZE_int_MR_", MR, "_restoration_FALSE_numb_0_level_0.csv"))
  file <- file %>% mutate(MR=MR, restoration=FALSE, restoration_num=0, restoration_MR_res=0)  
      
  assign(filename, file)
  
  remove(file)
  remove(filename)
}

## Base plots
theme_bw_alt <- theme_bw() %+replace%
  theme(
    linewidth = 0.3,
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank()
  )

theme_set(theme_bw_alt)


### Create dataframes

low_MR_df <- do.call(rbind, mget(grep("level_0.1", ls(), value = TRUE))) %>% 
  rbind(SIZE_int_MR_0.3_restoration_FALSE_numb_0_level_0, SIZE_int_MR_0.5_restoration_FALSE_numb_0_level_0, SIZE_int_MR_0.9_restoration_FALSE_numb_0_level_0)
mid_MR_df <- do.call(rbind, mget(grep("level_0.3", ls(), value = TRUE)))
high_MR_df <- do.call(rbind, mget(grep("level_0.5", ls(), value = TRUE)))

ylim_bott = min (high_MR_df[high_MR_df$time > 500,"sum_size"]-300)
ylim_top = max (low_MR_df$sum_size+500)

############## Base plots
#### High resistance seed lots

base_plot_lowMR_highrest <- ggplot() +
  geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
  geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_FALSE_numb_0_level_0, aes(x = time, y = sum_size, color = "NoRestoration"),method="gam",  se=F,span=100, linetype="dashed", linewidth=0.5) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_TRUE_numb_50_level_0.1, aes(x = time, y = sum_size, color = "LowRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_TRUE_numb_100_level_0.1, aes(x = time, y = sum_size, color = "MidRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_TRUE_numb_300_level_0.1, aes(x = time, y = sum_size, color = "HighRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + guides(colour = guide_legend(nrow=4,byrow=TRUE)) +
  scale_color_manual(
    values = c("LowRestoration"="chartreuse4","MidRestoration"="chartreuse3","HighRestoration"="chartreuse2",
               "NoRestoration"="grey50"),
    labels = c(
      "LowRestoration" = "50 indvs., Low MR site",
      "MidRestoration" = "100 indvs., Low MR site",
      "HighRestoration" = "300 indvs., Low MR site",
      "NoRestoration" = "No restoration"
    ),
    name = "Scenario"
  )
  
base_plot_midMR_highrest <- ggplot() +
  geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
  geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_FALSE_numb_0_level_0, aes(x = time, y = sum_size, color = "NoRestoration"),method="gam",  se=F,span=100, linetype="dashed", linewidth=0.5) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_TRUE_numb_50_level_0.1, aes(x = time, y = sum_size, color = "LowRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_TRUE_numb_100_level_0.1, aes(x = time, y = sum_size, color = "MidRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_TRUE_numb_300_level_0.1, aes(x = time, y = sum_size, color = "HighRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + guides(colour = guide_legend(nrow=4,byrow=TRUE)) +
  scale_color_manual(
    values = c("LowRestoration"="orange3","MidRestoration"="goldenrod2","HighRestoration"="gold",
               "NoRestoration"="grey50"),
    labels = c(
      "LowRestoration" = "50 indvs., Low MR site",
      "MidRestoration" = "100 indvs., Low MR site",
      "HighRestoration" = "300 indvs., Low MR site",
      "NoRestoration" = "No restoration"
    ),
    name = "Scenario"
  )

base_plot_highMR_highrest <- ggplot() +
  geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
  geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_FALSE_numb_0_level_0, aes(x = time, y = sum_size, color = "NoRestoration"),method="gam",  se=F,span=100, linetype="dashed", linewidth=0.5) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_TRUE_numb_50_level_0.1, aes(x = time, y = sum_size, color = "LowRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_TRUE_numb_100_level_0.1, aes(x = time, y = sum_size, color = "MidRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_TRUE_numb_300_level_0.1, aes(x = time, y = sum_size, color = "HighRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + guides(colour = guide_legend(nrow=4,byrow=TRUE)) +
  scale_color_manual(
    values = c("LowRestoration"="firebrick4","MidRestoration"="firebrick3","HighRestoration"="firebrick1",
               "NoRestoration"="grey50"),
    labels = c(
      "LowRestoration" = "50 indvs., Low MR site",
      "MidRestoration" = "100 indvs., Low MR site",
      "HighRestoration" = "300 indvs., Low MR site",
      "NoRestoration" = "No restoration"
    ),
    name = "Scenario"
  )
  
base_plot_highrest <- base_plot_highMR_highrest + base_plot_midMR_highrest + base_plot_lowMR_highrest
  
  
#####################
#### Mid resistance seed lots

base_plot_lowMR_midrest <- ggplot() +
  geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
  geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_FALSE_numb_0_level_0, aes(x = time, y = sum_size, color = "NoRestoration"),method="gam",  se=F,span=100, linetype="dashed", linewidth=0.5) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_TRUE_numb_50_level_0.3, aes(x = time, y = sum_size, color = "LowRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_TRUE_numb_100_level_0.3, aes(x = time, y = sum_size, color = "MidRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_TRUE_numb_300_level_0.3, aes(x = time, y = sum_size, color = "HighRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + guides(colour = guide_legend(nrow=4,byrow=TRUE)) +
  scale_color_manual(
    values = c("LowRestoration"="chartreuse4","MidRestoration"="chartreuse3","HighRestoration"="chartreuse2",
               "NoRestoration"="grey50"),
    labels = c(
      "LowRestoration" = "50 indvs., Low MR site",
      "MidRestoration" = "100 indvs., Low MR site",
      "HighRestoration" = "300 indvs., Low MR site",
      "NoRestoration" = "No restoration"
    ),
    name = "Scenario"
  )

base_plot_midMR_midrest <- ggplot() +
  geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
  geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_FALSE_numb_0_level_0, aes(x = time, y = sum_size, color = "NoRestoration"),method="gam",  se=F,span=100, linetype="dashed", linewidth=0.5) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_TRUE_numb_50_level_0.3, aes(x = time, y = sum_size, color = "LowRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_TRUE_numb_100_level_0.3, aes(x = time, y = sum_size, color = "MidRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_TRUE_numb_300_level_0.3, aes(x = time, y = sum_size, color = "HighRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + guides(colour = guide_legend(nrow=4,byrow=TRUE)) +
  scale_color_manual(
    values = c("LowRestoration"="orange3","MidRestoration"="goldenrod2","HighRestoration"="gold",
               "NoRestoration"="grey50"),
    labels = c(
      "LowRestoration" = "50 indvs., Low MR site",
      "MidRestoration" = "100 indvs., Low MR site",
      "HighRestoration" = "300 indvs., Low MR site",
      "NoRestoration" = "No restoration"
    ),
    name = "Scenario"
  )

base_plot_highMR_midrest <- ggplot() +
  geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
  geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_FALSE_numb_0_level_0, aes(x = time, y = sum_size, color = "NoRestoration"),method="gam",  se=F,span=100, linetype="dashed", linewidth=0.5) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_TRUE_numb_50_level_0.3, aes(x = time, y = sum_size, color = "LowRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_TRUE_numb_100_level_0.3, aes(x = time, y = sum_size, color = "MidRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_TRUE_numb_300_level_0.3, aes(x = time, y = sum_size, color = "HighRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + guides(colour = guide_legend(nrow=4,byrow=TRUE)) +
  scale_color_manual(
    values = c("LowRestoration"="firebrick4","MidRestoration"="firebrick3","HighRestoration"="firebrick1",
               "NoRestoration"="grey50"),
    labels = c(
      "LowRestoration" = "50 indvs., Low MR site",
      "MidRestoration" = "100 indvs., Low MR site",
      "HighRestoration" = "300 indvs., Low MR site",
      "NoRestoration" = "No restoration"
    ),
    name = "Scenario"
  )

base_plot_midrest <- base_plot_highMR_midrest + base_plot_midMR_midrest + base_plot_lowMR_midrest

####################
#### Low resistance seed lots

base_plot_lowMR_lowrest <- ggplot() +
  geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
  geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_FALSE_numb_0_level_0, aes(x = time, y = sum_size, color = "NoRestoration"),method="gam",  se=F,span=100, linetype="dashed", linewidth=0.5) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_TRUE_numb_50_level_0.5, aes(x = time, y = sum_size, color = "LowRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_TRUE_numb_100_level_0.5, aes(x = time, y = sum_size, color = "MidRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.3_restoration_TRUE_numb_300_level_0.5, aes(x = time, y = sum_size, color = "HighRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + guides(colour = guide_legend(nrow=4,byrow=TRUE)) +
  scale_color_manual(
    values = c("LowRestoration"="chartreuse4","MidRestoration"="chartreuse3","HighRestoration"="chartreuse2",
               "NoRestoration"="grey50"),
    labels = c(
      "LowRestoration" = "50 indvs., Low MR site",
      "MidRestoration" = "100 indvs., Low MR site",
      "HighRestoration" = "300 indvs., Low MR site",
      "NoRestoration" = "No restoration"
    ),
    name = "Scenario"
  )

base_plot_midMR_lowrest <- ggplot() +
  geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
  geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_FALSE_numb_0_level_0, aes(x = time, y = sum_size, color = "NoRestoration"),method="gam",  se=F,span=100, linetype="dashed", linewidth=0.5) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_TRUE_numb_50_level_0.5, aes(x = time, y = sum_size, color = "LowRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_TRUE_numb_100_level_0.5, aes(x = time, y = sum_size, color = "MidRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.5_restoration_TRUE_numb_300_level_0.5, aes(x = time, y = sum_size, color = "HighRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + guides(colour = guide_legend(nrow=4,byrow=TRUE)) +
  scale_color_manual(
    values = c("LowRestoration"="orange3","MidRestoration"="goldenrod2","HighRestoration"="gold",
               "NoRestoration"="grey50"),
    labels = c(
      "LowRestoration" = "50 indvs., Low MR site",
      "MidRestoration" = "100 indvs., Low MR site",
      "HighRestoration" = "300 indvs., Low MR site",
      "NoRestoration" = "No restoration"
    ),
    name = "Scenario"
  )

base_plot_highMR_lowrest <- ggplot() +
  geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
  geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_FALSE_numb_0_level_0, aes(x = time, y = sum_size, color = "NoRestoration"),method="gam",  se=F,span=100, linetype="dashed", linewidth=0.5) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_TRUE_numb_50_level_0.5, aes(x = time, y = sum_size, color = "LowRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_TRUE_numb_100_level_0.5, aes(x = time, y = sum_size, color = "MidRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  stat_smooth(data=SIZE_int_MR_0.9_restoration_TRUE_numb_300_level_0.5, aes(x = time, y = sum_size, color = "HighRestoration"),method="gam",  se=F,span=100, linewidth=0.3) +
  xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + guides(colour = guide_legend(nrow=4,byrow=TRUE)) +
  scale_color_manual(
    values = c("LowRestoration"="firebrick4","MidRestoration"="firebrick3","HighRestoration"="firebrick1",
               "NoRestoration"="grey50"),
    labels = c(
      "LowRestoration" = "50 indvs., Low MR site",
      "MidRestoration" = "100 indvs., Low MR site",
      "HighRestoration" = "300 indvs., Low MR site",
      "NoRestoration" = "No restoration"
    ),
    name = "Scenario"
  )

base_plot_lowrest <- base_plot_highMR_lowrest + base_plot_midMR_lowrest + base_plot_lowMR_lowrest

####################

ggsave(base_plot_lowrest, file=paste0(base_dir, "Low_rest_baseplot.jpg"), width=3500, height=2000, units="px", limitsize=FALSE)
ggsave(base_plot_midrest, file=paste0(base_dir, "Mid_rest_baseplot.jpg"), width=3500, height=2000, units="px", limitsize=FALSE)
ggsave(base_plot_highrest, file=paste0(base_dir, "High_rest_baseplot.jpg"), width=3500, height=2000, units="px", limitsize=FALSE)
       
####################

## Tailored plots
plot_MR_res_vary <- function(MR_res, MR_press, MR_res_name, MR_press_name, restoration_numb) {
  
  df_list <- list()
  for (restoration_num in restoration_numb) {
    file_name <- paste0("SIZE_int_MR_", MR_press, "_restoration_TRUE_numb_", restoration_num, "_level_", MR_res)
    df_list[[as.character(restoration_num)]] <- get(file_name)
  }
  
  # Add the FALSE restoration (baseline)
  df_false <- get(paste0("SIZE_int_MR_", MR_press, "_restoration_FALSE_numb_0_level_0"))
  
  # Start plot
  plot <- ggplot() +
    geom_vline(xintercept=1000, linewidth = 0.5, linetype="dashed", colour="darkorchid4", alpha=0.5) +
    geom_vline(xintercept=1025, linewidth = 0.5, linetype="dashed", colour="orchid", alpha=0.5) +
    xlim(c(500, 2000))+ ylim(c(ylim_bott,ylim_top)) +
    stat_smooth(data = df_false, aes(x = time, y = sum_size, colour = "No restoration"), linewidth = 0.4, span = 10, se = FALSE)
  for (restoration_num in restoration_numb) {
    plot <- plot +
      stat_smooth(data = df_list[[as.character(restoration_num)]],
                  aes(x = time, y = sum_size, colour = paste0(restoration_num, " indvs.")),
                  linewidth = 0.4, span = 10, se = FALSE)
  }
  
  # Define colours and legend
  plot <- plot +
    scale_colour_manual(
      name = "Scenario",
      values = c(
        "No restoration" = "grey",
        "50 indvs." = "goldenrod2",
        "100 indvs." = "forestgreen",
        "300 indvs." = "royalblue"
      )
    ) 
  
  # Save plot
  plot_name <- paste0(base_dir, MR_res_name, "rest_", MR_press_name, "_MR_variednumb.jpg")
  ggsave(plot_name, plot = plot, width = 2500, height = 2000, units = "px", limitsize = FALSE)
  
  return(plot)
}


# High MR press
plot_MR_res_vary(0.1, 0.9, "low", "high", restoration_numb)
plot_MR_res_vary(0.3, 0.9, "mid", "high", restoration_numb)
plot_MR_res_vary(0.5, 0.9, "high", "high", restoration_numb)

# Mid MR press
plot_MR_res_vary(0.1, 0.5, "low", "mid", restoration_numb)
plot_MR_res_vary(0.3, 0.5, "mid", "mid", restoration_numb)
plot_MR_res_vary(0.5, 0.5, "high", "mid", restoration_numb)

# Low MR press
plot_MR_res_vary(0.1, 0.3, "low", "low", restoration_numb)
plot_MR_res_vary(0.3, 0.3, "mid", "low", restoration_numb)
plot_MR_res_vary(0.5, 0.3, "high", "low", restoration_numb)

