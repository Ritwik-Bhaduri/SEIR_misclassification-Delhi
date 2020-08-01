library("dplyr")
library(latex2exp)
library(ggplot2)
library(ggpubr)

prediction_0 = rowMeans(readRDS(file.choose()))
prediction_0.15 = rowMeans(readRDS(file.choose()))
prediction_0.3 = rowMeans(readRDS(file.choose()))

data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))
state = "DL"
data = data_state[, c("Date", "Status", state)]
daily_confirmed = filter(data, Status %in% "Confirmed")[,3]
daily_recovered = filter(data, Status %in% "Recovered")[,3]
daily_deceased = filter(data, Status %in% "Deceased")[,3]
date = as.character(filter(data, Status %in% "Confirmed")[,1])
data = data.frame(date = date, "Daily.Confirmed" = daily_confirmed, 
                  "Daily.Recovered" = daily_recovered, "Daily.Deceased" = daily_deceased)
date_1_june = which(data$date == "01-Jun-20")
date_26_july = min(which(data$date == "26-Jul-20"), length(daily_confirmed))
data_initial = c(20834, 8746, 543)
data = data[date_1_june:date_26_july, ]
obsP_daily <- data[,"Daily.Confirmed"] ## Daily Positive
obsR_daily <- data[,"Daily.Recovered"] ## Daily Recovered
obsD_daily <- data[,"Daily.Deceased"]  ##  Daily Deaths
obsP_total <- cumsum(obsP_daily) + data_initial[1] 
obsR_total <- cumsum(obsR_daily) + data_initial[2] 
obsD_total <- cumsum(obsD_daily) + data_initial[3] 
obsP_current <- obsP_total - obsR_total - obsD_total 
date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")

########################################################################################################
## Themes and colors for plot
########################################################################################################
## Set colors for plot

panel_colors <- c("Predicted Total" = "#a8b0b5", "Predicted Reported" = "#173F5F", "Observed" = "#ED553B")
lineplot_colors <- c("FNR_0.3" ="#3CAEA3", "FNR_0.15" = "#0472cf", "FNR_0" = "#173F5F", "Observed" = "#ED553B")

## Themes for plot

max_theme <-   theme_minimal() +
  theme(
    # text               = element_text(family = "Arial"),
    plot.title         = element_text(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = element_text(hjust = 0, size = 10, lineheight = 1.1),
    axis.text          = element_text(size = 16, color = "#36454f"),
    axis.title         = element_text(size = 16, face = "italic"),
    legend.title       = element_blank() ,
    legend.text        = element_text(size = 16) ,
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom"
  )

max_theme_panel <-   theme_minimal() +
  theme(
    # text               = element_text(family = "Arial"),
    plot.title         = element_text(size = 16, face = "bold"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = element_text(hjust = 0, size = 10, lineheight = 1.1),
    axis.text          = element_text(size = 14, color = "#36454f"),
    axis.title         = element_text(size = 14, face = "italic"),
    legend.title       = element_blank() ,
    legend.text        = element_text(size = 14) ,
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom"
  )

## Function for modifying axis labels

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}


########################################################################################################
## Plot 1: 2 panel showing misclassification effect for deaths and cases  
########################################################################################################

total_pred_0 = rowSums(matrix(prediction_0, nrow = 201)[79:134, 3:9])
total_pred_0.15 = rowSums(matrix(prediction_0.15, nrow = 201)[79:134, 3:9])
total_pred_0.3 = rowSums(matrix(prediction_0.3, nrow = 201)[79:134, 3:9])

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")
data_compare = data.frame("date" = date, "x1" = total_pred_0, "x2" = total_pred_0.15, "x3" = total_pred_0.3, "obs" = obsP_total)

lineplot_colors <- c("FNR_0.3" ="#3CAEA3", "FNR_0.15" = "#0472cf", "FNR_0" = "#173F5F", "Observed" = "#ED553B")

lineplot_colors1 <- c("FNR_0.3" ="#3CAEA3", "FNR_0.15" = "#0472cf", "FNR_0" = "#173F5F", "Observed" = "#ED553B")[c(4,1,2,3)]
lineplot_colors2 <- c("FNR_0.3" ="#3CAEA3", "FNR_0.15" = "#0472cf", "FNR_0" = "#173F5F", "Observed" = "#ED553B")
lineplot_colors3 <- c("FNR_0.3" ="#3CAEA3", "FNR_0.15" = "#0472cf", "FNR_0" = "#173F5F", "Observed" = "#ED553B")[c(3,2,1,4)]
png("effect of misclassification on case.png", units = "in", width = 10, height = 7, res = 300)
p1 <- ggplot(data_compare, aes(x = date)) + 
  geom_line(aes(y = x1, color = "Sensitivity = 1"), alpha = 1, size = 1.2)+ 
  geom_line(aes(y = x2, color = "Sensitivity = 0.85"), alpha = 1, size = 1.2)+ 
  geom_line(aes(y = x3, color = "Sensitivity = 0.7"), alpha = 1, size = 1.2)+ 
  geom_line(aes(y = obs, color = "Observed"), alpha = 1, size = 1.2, linetype = "dashed")+ 
  labs(title="Total Predicted Cases For Different Values Of Sensitivity",x="", y = "") +
  max_theme + 
  scale_color_manual(values = as.vector(lineplot_colors1)) +
  theme(plot.margin = unit(c(1,5,1,1), "lines")) +
  scale_y_continuous(labels = addUnits) + 
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_segment(aes(x = as.Date("2020-07-10"), y = -30, xend = as.Date("2020-07-10"), yend = total_pred_0.3[40]), size = 0.7, linetype = "dashed", color = "darkgray")+
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-10"), y = total_pred_0[40], xend = as.Date("2020-07-16"), yend = total_pred_0[40]*0.9), color = lineplot_colors3[1]) +
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-16"), y = total_pred_0[40]*0.9, xend = as.Date("2020-07-26"), yend = total_pred_0[40]*0.9), color = lineplot_colors3[1]) +
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-10"), y = total_pred_0.15[40], xend = as.Date("2020-07-26"), yend = total_pred_0.15[40]), color = lineplot_colors3[2]) +
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-10"), y = total_pred_0.3[40], xend = as.Date("2020-07-16"), yend = total_pred_0.3[40]*1.07), color = lineplot_colors3[3]) + 
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-16"), y = total_pred_0.3[40]*1.07, xend = as.Date("2020-07-26"), yend = total_pred_0.3[40]*1.07), color = lineplot_colors3[3]) + 
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-10"), y = obs[40], xend = as.Date("2020-07-16"), yend = obs[40]*15), color = lineplot_colors3[4]) + 
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-16"), y = obs[40]*15, xend = as.Date("2020-07-26"), yend = obs[40]*15), color = lineplot_colors3[4]) + 
  annotate("label", x = as.Date("2020-07-26"), y = c(total_pred_0[40]*0.9,total_pred_0.15[40]*1.02,total_pred_0.3[40]*1.07, obsP_total[40]*15), 
           label = c(paste0(round(total_pred_0[40]*1e-6,1), " Million \n (34.8x)"), paste0(round(total_pred_0.15[40]*1e-6,1), " Million \n (39.8x)"), paste0(round(total_pred_0.3[40]*1e-6,1), " Million \n (52.6x)"),paste0(109140)),
           color = lineplot_colors3, size = 6, hjust = 'left')
gt1 <- ggplot_gtable(ggplot_build(p1))
gt1$layout$clip[gt1$layout$name == "panel"] <- "off"
grid::grid.draw(gt1)
dev.off()

## deaths on 10 july : 3300
total_pred_death_0 = rowSums(matrix(prediction_0, nrow = 201)[79:134, 8:9])
total_pred_death_0.15 = rowSums(matrix(prediction_0.15, nrow = 201)[79:134, 8:9])
total_pred_death_0.3 = rowSums(matrix(prediction_0.3, nrow = 201)[79:134, 8:9])

daily_pred_death_0 = c(2*diff(total_pred_death_0)[1]-diff(total_pred_death_0)[2], diff(total_pred_death_0))
daily_pred_death_0.15 = c(2*diff(total_pred_death_0.15)[1]-diff(total_pred_death_0.15)[2], diff(total_pred_death_0.15))
daily_pred_death_0.3 = c(2*diff(total_pred_death_0.3)[1]-diff(total_pred_death_0.3)[2], diff(total_pred_death_0.3))

total_detected_death_0 = matrix(prediction_0, nrow = 201)[79:134, 9]
total_detected_death_0.15 = matrix(prediction_0.15, nrow = 201)[79:134, 9]
total_detected_death_0.3 = matrix(prediction_0.3, nrow = 201)[79:134, 9]

daily_detected_death_0 = c(2*diff(total_detected_death_0)[1]-diff(total_detected_death_0)[2], diff(total_detected_death_0))
daily_detected_death_0.15 = c(2*diff(total_detected_death_0.15)[1]-diff(total_detected_death_0.15)[2], diff(total_detected_death_0.15))
daily_detected_death_0.3 = c(2*diff(total_detected_death_0.3)[1]-diff(total_detected_death_0.3)[2], diff(total_detected_death_0.3))

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")
data_compare_death = data.frame("date" = date, "x1" = total_pred_death_0, "x2" = total_pred_death_0.15, "x3" = total_pred_death_0.3, "obs" = obsD_total)

lineplot_colors1 <- c("FNR_0.3" ="#3CAEA3", "FNR_0.15" = "#0472cf", "FNR_0" = "#173F5F", "Observed" = "#ED553B")[c(4,1,2,3)]
lineplot_colors2 <- c("FNR_0.3" ="#3CAEA3", "FNR_0.15" = "#0472cf", "FNR_0" = "#173F5F", "Observed" = "#ED553B")[c(3,2,1,4)]
lineplot_colors3 <- c("FNR_0.3" ="#3CAEA3", "FNR_0.15" = "#0472cf", "FNR_0" = "#173F5F", "Observed" = "#ED553B")[c(3,2,1,4)]

library(ggplot2)
library(ggpubr)
png("effect of misclassification on death.png", units = "in", width = 10, height = 7, res = 300)
p2 <- ggplot(data_compare_death, aes(x = date)) + 
  geom_line(aes(y = x1, color = "Sensitivity = 1"), alpha = 1, size = 1.2)+ 
  geom_line(aes(y = x2, color = "Sensitivity = 0.85"), alpha = 1, size = 1.2)+ 
  geom_line(aes(y = x3, color = "Sensitivity = 0.7"), alpha = 1, size = 1.2)+ 
  geom_line(aes(y = obs, color = "Observed"), alpha = 1, size = 1.2, linetype = "dashed")+ 
  max_theme + 
  scale_color_manual(values = as.vector(lineplot_colors1))+
  labs(title="Total Predicted Deaths For Different Values Of Sensitivity",x="", y = "") +
  labs(color = "False Negative Rates") + 
  theme(plot.margin = unit(c(1,5,1,1), "lines")) + 
  scale_y_continuous(labels = addUnits) + 
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_segment(aes(x = as.Date("2020-07-10"), y = -30, xend = as.Date("2020-07-10"), yend = total_pred_death_0.3[40]), size = 0.7, linetype = "dashed", color = "darkgray")+
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-10"), y = total_pred_death_0[40], xend = as.Date("2020-07-26"), yend = total_pred_death_0[40]), color = lineplot_colors2[1]) +
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-10"), y = total_pred_death_0.15[40], xend = as.Date("2020-07-16"), yend = total_pred_death_0.15[40]*1.15), color = lineplot_colors2[2]) +
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-16"), y = total_pred_death_0.15[40]*1.15, xend = as.Date("2020-07-26"), yend = total_pred_death_0.15[40]*1.15), color = lineplot_colors2[2]) +
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-10"), y = total_pred_death_0.3[40], xend = as.Date("2020-07-21"), yend = total_pred_death_0.3[40]*1.2), color = lineplot_colors2[3]) + 
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-21"), y = total_pred_death_0.3[40]*1.2, xend = as.Date("2020-07-26"), yend = total_pred_death_0.3[40]*1.2), color = lineplot_colors2[3]) + 
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-10"), y = obs[40], xend = as.Date("2020-07-16"), yend = obs[40]*5), color = lineplot_colors2[4]) + 
  geom_segment(lineend = 'round', linejoin = 'round', aes(x = as.Date("2020-07-16"), y = obs[40]*5, xend = as.Date("2020-07-26"), yend = obs[40]*5), color = lineplot_colors2[4]) + 
  annotate("label", x = as.Date("2020-07-26"), y = c(total_pred_death_0[40]*0.98,total_pred_death_0.15[40]*1.15,total_pred_death_0.3[40]*1.2, obsD_total[40]*5), 
           label = c(paste0(round(total_pred_death_0[40],0), "\n (8.4x)"), paste0(round(total_pred_death_0.15[40],0), "\n (9.8x)"), paste0(round(total_pred_death_0.3[40],0), "\n (12.9x)"),paste0(3300)),
           color = lineplot_colors3, size = 6, hjust = 'left')
p2
gt2 <- ggplot_gtable(ggplot_build(p2))
gt2$layout$clip[gt2$layout$name == "panel"] <- "off"
grid::grid.draw(gt2)
dev.off()

extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
shared_legend <- extract_legend(p1)
gt1 <- ggplot_gtable(ggplot_build(p1+theme(legend.position = "none")))
gt1$layout$clip[gt1$layout$name == "panel"] <- "off"
gt2 <- ggplot_gtable(ggplot_build(p2+theme(legend.position = "none")))
gt2$layout$clip[gt2$layout$name == "panel"] <- "off"

library(cowplot)
png("effect_of_misclassification_delhi_panel.png", units = "in", width = 18, height = 8, res = 300)
grid_plot = plot_grid(gt1, gt2, ncol=2, labels=c("A", "B"), label_size = 28)
grid_plot = grid_plot + draw_grob(shared_legend, 0, 0, 1, -0.05) + theme(plot.margin = unit(c(1,1,2,1), "lines"))
grid_plot
dev.off()

ggsave("effect_of_misclassification_delhi_panel.pdf", grid_plot, width = 18, height = 8, units = "in", dpi = 300)


########################################################################################################################3
## Plot 2 : 4 panel for cases
########################################################################################################################3
total_pred_0 = rowSums(matrix(prediction_0, nrow = 201)[79:134, 3:9])
total_pred_0.15 = rowSums(matrix(prediction_0.15, nrow = 201)[79:134, 3:9])
total_pred_0.3 = rowSums(matrix(prediction_0.3, nrow = 201)[79:134, 3:9])

total_detected_0 = rowSums(matrix(prediction_0, nrow = 201)[79:134, c(4,7,9)])
total_detected_0.15 = rowSums(matrix(prediction_0.15, nrow = 201)[79:134, c(4,7,9)])
total_detected_0.3 = rowSums(matrix(prediction_0.3, nrow = 201)[79:134, c(4,7,9)])

daily_pred_0 = c(2*diff(total_pred_0)[1]-diff(total_pred_0)[2], diff(total_pred_0))
daily_pred_0.15 = c(2*diff(total_pred_0.15)[1]-diff(total_pred_0.15)[2], diff(total_pred_0.15))
daily_pred_0.3 = c(2*diff(total_pred_0.3)[1]-diff(total_pred_0.3)[2], diff(total_pred_0.3))

daily_detected_0 = c(2*diff(total_detected_0)[1]-diff(total_detected_0)[2], diff(total_detected_0))
daily_detected_0.15 = c(2*diff(total_detected_0.15)[1]-diff(total_detected_0.15)[2], diff(total_detected_0.15))
daily_detected_0.3 = c(2*diff(total_detected_0.3)[1]-diff(total_detected_0.3)[2], diff(total_detected_0.3))

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")

panel_colors <- as.vector(c("Predicted Total" = "#a8b0b5", "Predicted Reported" = "#173F5F", "Observed" = "#ED553B"))[c(2,1,3)]

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")
data_daily_0 = data.frame(date = date, "daily" = daily_pred_0, "detected" = daily_detected_0, "obs" = obsP_daily)
png("daily case delhi f = 0.png", units = "in", width = 10, height = 7, res = 300)
p1 <- ggplot(data_daily_0, aes(x = date)) +
  geom_col(aes(y = daily, fill = "Predicted Unreported"), alpha = 1) +
  geom_col(aes(y = detected, fill = "Predicted Reported"), alpha = 1) +
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_line(aes(y = obs, color = "Observed"), size = 1.2) + 
  geom_vline(xintercept = as.Date("2020-07-10"), col = rgb(0.3,0.3,0.3)) +
  scale_fill_manual(values = panel_colors[c(1,2)]) +
  scale_color_manual(values = as.vector(panel_colors[3])) + 
  max_theme_panel +
  labs(title="Daily cases : Sensitivity = 1", x = "", y = "") +
  labs(fill = "") + 
  scale_y_continuous(labels = addUnits, limits = c(0, max(c(daily_pred_0, daily_pred_0.15, daily_pred_0.3))))
p1
dev.off()

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")
data_daily_0.15 = data.frame(date = date, "daily" = daily_pred_0.15, "detected" = daily_detected_0.15, "obs" = obsP_daily)
png("daily case delhi f = 0.15.png", units = "in", width = 10, height = 7, res = 300)
p2 <- ggplot(data_daily_0.15, aes(x = date)) +
  geom_col(aes(y = daily, fill = "Predicted Unreported"), alpha = 1) +
  geom_col(aes(y = detected, fill = "Predicted Reported"), alpha = 1) +
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_line(aes(y = obs, color = "Observed"), size = 1.2) + 
  geom_vline(xintercept = as.Date("2020-07-10"), col = rgb(0.3,0.3,0.3)) +
  scale_fill_manual(values = panel_colors[c(1,2)]) +
  scale_color_manual(values = as.vector(panel_colors[3])) + 
  max_theme_panel +
  labs(title="Daily cases : Sensitivity = 0.85", x = "", y = "") +
  labs(fill = "") + 
  scale_y_continuous(labels = addUnits, limits = c(0, max(c(daily_pred_0, daily_pred_0.15, daily_pred_0.3))))
p2
dev.off()

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")
data_daily_0.3 = data.frame(date = date, "daily" = daily_pred_0.3, "detected" = daily_detected_0.3, "obs" = obsP_daily)
png("daily case delhi f = 0.3.png", units = "in", width = 10, height = 7, res = 300)
p3 <- ggplot(data_daily_0.3, aes(x = date)) +
  geom_col(aes(y = daily, fill = "Predicted Unreported"), alpha = 1) +
  geom_col(aes(y = detected, fill = "Predicted Reported"), alpha = 1) +
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_line(aes(y = obs, color = "Observed"), size = 1.2) + 
  geom_vline(xintercept = as.Date("2020-07-10"), col = rgb(0.3,0.3,0.3)) +
  scale_fill_manual(values = panel_colors[c(1,2)]) +
  scale_color_manual(values = as.vector(panel_colors[3])) + 
  max_theme_panel +
  labs(title="Daily cases : Sensitivity = 0.7", x = "", y = "") +
  labs(fill = "") + 
  scale_y_continuous(labels = addUnits, limits = c(0, max(c(daily_pred_0, daily_pred_0.15, daily_pred_0.3))))
p3
dev.off()

library(colorspace)
data_daily_observed = data.frame(date = date, "predicted" = daily_detected_0.3, "obs" = obsP_daily)
png("daily case delhi observed.png", units = "in", width = 10, height = 7, res = 300)
p4 <- ggplot(data_daily_observed, aes(x = date)) +
  geom_col(aes(y = predicted, fill = "Predicted Reported"), alpha = 1) +
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_line(aes(y = obs, color = "Observed"), size = 2) + 
  geom_vline(xintercept = as.Date("2020-07-10"), col = rgb(0.3,0.3,0.3)) +
  scale_fill_manual(values = panel_colors[1]) +
  scale_color_manual(values =as.vector(panel_colors[3])) + 
  max_theme_panel +
  labs(title="Daily COVID cases in Delhi", x = "", y = "") +
  labs(fill = "") + 
  scale_y_continuous(labels = addUnits)
p4
dev.off()

png("daily case delhi.png", units = "in", width = 12, height = 9, res = 300)
grid_plot = ggarrange(p3, p2, p1, p4, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), font.label = list(size = 20), common.legend = TRUE, legend = "bottom")
grid_plot + theme(plot.margin = unit(c(0,1,0,0), "lines"))
dev.off()


ggsave("daily case delhi.pdf", grid_plot, width = 12, height = 9, units = "in", dpi = 300)


#######################################################################################################################
## Plot3 : 3 panel showing proportion of detected for deaths
#######################################################################################################################
total_pred_death_0 = rowSums(matrix(prediction_0, nrow = 201)[79:134, 8:9])
total_pred_death_0.15 = rowSums(matrix(prediction_0.15, nrow = 201)[79:134, 8:9])
total_pred_death_0.3 = rowSums(matrix(prediction_0.3, nrow = 201)[79:134, 8:9])

total_detected_death_0 = (matrix(prediction_0, nrow = 201)[79:134, c(9)])
total_detected_death_0.15 = (matrix(prediction_0.15, nrow = 201)[79:134, c(9)])
total_detected_death_0.3 = (matrix(prediction_0.3, nrow = 201)[79:134, c(9)])

daily_pred_death_0 = c(2*diff(total_pred_death_0)[1]-diff(total_pred_death_0)[2], diff(total_pred_death_0))
daily_pred_death_0.15 = c(2*diff(total_pred_death_0.15)[1]-diff(total_pred_death_0.15)[2], diff(total_pred_death_0.15))
daily_pred_death_0.3 = c(2*diff(total_pred_death_0.3)[1]-diff(total_pred_death_0.3)[2], diff(total_pred_death_0.3))

daily_detected_death_0 = c(2*diff(total_detected_death_0)[1]-diff(total_detected_death_0)[2], diff(total_detected_death_0))
daily_detected_death_0.15 = c(2*diff(total_detected_death_0.15)[1]-diff(total_detected_death_0.15)[2], diff(total_detected_death_0.15))
daily_detected_death_0.3 = c(2*diff(total_detected_death_0.3)[1]-diff(total_detected_death_0.3)[2], diff(total_detected_death_0.3))

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")
panel_colors <- as.vector(c("Predicted Total" = "#a8b0b5", "Predicted Reported" = "#173F5F", "Observed" = "#ED553B"))[c(2,1,3)]

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")
data_daily_0 = data.frame(date = date, "daily" = daily_pred_death_0, "detected" = daily_detected_death_0, "obs" = obsD_daily)
png("daily death delhi f = 0.png", units = "in", width = 10, height = 7, res = 300)
p1 <- ggplot(data_daily_0, aes(x = date)) +
  geom_col(aes(y = daily, fill = "Predicted Unreported"), alpha = 1) +
  geom_col(aes(y = detected, fill = "Predicted Reported"), alpha = 1) +
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_line(aes(y = obs, color = "Observed"), size = 1.2) + 
  geom_vline(xintercept = as.Date("2020-07-10"), col = rgb(0.3,0.3,0.3)) +
  scale_fill_manual(values = panel_colors[c(1,2)]) +
  scale_color_manual(values = panel_colors[3]) + 
  max_theme_panel +
  labs(title="Daily deaths : Sensitivity = 1", x = "", y = "") +
  labs(fill = "") + 
  scale_y_continuous(labels = addUnits, limits = c(0, max(c(daily_pred_death_0, daily_pred_death_0.15, daily_pred_death_0.3))))
p1
dev.off()

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")
data_daily_0.15 = data.frame(date = date, "daily" = daily_pred_death_0.15, "detected" = daily_detected_death_0.15, "obs" = obsD_daily)
png("daily death delhi f = 0.15.png", units = "in", width = 10, height = 7, res = 300)
p2 <- ggplot(data_daily_0.15, aes(x = date)) +
  geom_col(aes(y = daily, fill = "Predicted Unreported"), alpha = 1) +
  geom_col(aes(y = detected, fill = "Predicted Reported"), alpha = 1) +
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_line(aes(y = obs, color = "Observed"), size = 1.2) + 
  geom_vline(xintercept = as.Date("2020-07-10"), col = rgb(0.3,0.3,0.3)) +
  scale_fill_manual(values = panel_colors[c(1,2)]) +
  scale_color_manual(values = panel_colors[3]) + 
  max_theme_panel +
  labs(title="Daily deaths : Sensitivity = 0.85", x = "", y = "") +
  labs(fill = "") + 
  scale_y_continuous(labels = addUnits, limits = c(0, max(c(daily_pred_death_0, daily_pred_death_0.15, daily_pred_death_0.3))))
p2
dev.off()

date = seq(as.Date("2020-06-1"), as.Date("2020-07-26"), by="days")
data_daily_0.3 = data.frame(date = date, "daily" = daily_pred_death_0.3, "detected" = daily_detected_death_0.3, "obs" = obsD_daily)
png("daily death delhi f = 0.3.png", units = "in", width = 10, height = 7, res = 300)
p3 <- ggplot(data_daily_0.3, aes(x = date)) +
  geom_col(aes(y = daily, fill = "Predicted Unreported"), alpha = 1) +
  geom_col(aes(y = detected, fill = "Predicted Reported"), alpha = 1) +
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_line(aes(y = obs, color = "Observed"), size = 1.2) + 
  geom_vline(xintercept = as.Date("2020-07-10"), col = rgb(0.3,0.3,0.3)) +
  scale_fill_manual(values = panel_colors[c(1,2)]) +
  scale_color_manual(values = panel_colors[3]) + 
  max_theme_panel +
  labs(title="Daily deaths : Sensitivity = 0.7", x = "", y = "") +
  labs(fill = "") + 
  scale_y_continuous(labels = addUnits, limits = c(0, max(c(daily_pred_death_0, daily_pred_death_0.15, daily_pred_death_0.3))))
p3
dev.off()

library(colorspace)
data_daily_observed = data.frame(date = date, "predicted" = daily_detected_death_0.3, "obs" = obsD_daily)
png("daily death delhi observed.png", units = "in", width = 10, height = 7, res = 300)
p4 <- ggplot(data_daily_observed, aes(x = date)) +
  geom_col(aes(y = predicted, fill = "Predicted Reported"), alpha = 1) +
  scale_x_date(breaks = as.Date(c("2020-06-1","2020-06-15","2020-07-1","2020-07-10","2020-07-26")), 
               labels = c(TeX("$1^{st} June$"), TeX("$15^{th} June$"),TeX("$1^{st} July$"),TeX("$10^{th} July$"), TeX("$26^{th} July$"))) +   
  geom_line(aes(y = obs, color = "Observed"), size = 2) + 
  geom_vline(xintercept = as.Date("2020-07-10"), col = rgb(0.3,0.3,0.3)) +
  scale_fill_manual(values = panel_colors[1]) +
  scale_color_manual(values =panel_colors[3]) + 
  max_theme_panel +
  labs(title="Daily COVID deaths in Delhi", x = "", y = "") +
  labs(fill = "") + 
  scale_y_continuous(labels = addUnits)
p4
dev.off()

png("daily death delhi.png", units = "in", width = 12, height = 9, res = 300)
grid_plot = ggarrange(p3, p2, p1, p4, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), font.label = list(size = 20), common.legend = TRUE, legend = "bottom")
grid_plot
dev.off()


ggsave("daily death delhi.pdf", grid_plot, width = 12, height = 9, units = "in", dpi = 300)

