### Site sensitivity model summaries ###
###         Working across sites         ###

## Data manipulation packages
library(tidyverse)
library(googledrive)#read this tutorial to setup the Google account permissions https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package
library(readxl)
#Create directory for file download 
# I put this directory in to the .gitignore so it does not affect the repository when changes are pushed
dir.create("raw_data_GDrive", showWarnings = F)

#Create a tibble with all of the files in the LTER transitions directory
#Don't need to change this since this is where all of the raw files are located
files_ls=drive_ls(as_id("https://drive.google.com/drive/folders/1xkxWpWA9NEeEoUmZfkRMsS9s7zodo7xU"))

#Google asks for access to the pre-authorized Google account 
#my account is under "1" yours may be different 


#Download the dataset based on the file name in the directory
drive_download(file = subset(files_ls,name=="Climate sensitivity"),
               path = "raw_data_GDrive/Climate_sensitivity.xlsx",
               overwrite = TRUE,
               type="xlsx")#Overwrite is in included so you can replace older versions



Climate_sensitivity_MATlog<-read_xlsx("raw_data_GDrive/Climate_sensitivity.xlsx", sheet = "Log_logAnnual_means")
colnames(Climate_sensitivity_MATlog)<-str_replace_all(colnames(Climate_sensitivity_MATlog)," ","_")
colnames(Climate_sensitivity_MATlog)

Climate_sensitivity_log<-read_xlsx("raw_data_GDrive/Climate_sensitivity.xlsx", sheet = "Log_logMaxTemp")
colnames(Climate_sensitivity_log)<-str_replace_all(colnames(Climate_sensitivity_log)," ","_")
colnames(Climate_sensitivity_log)


Climate_sensitivity_scale<-read_xlsx("raw_data_GDrive/Climate_sensitivity.xlsx", sheet = "Scaled")
colnames(Climate_sensitivity_scale)<-str_replace_all(colnames(Climate_sensitivity_scale)," ","_")
colnames(Climate_sensitivity_scale)


#Log-log Mean Temp----




ggplot(Climate_sensitivity_MATlog, aes(x = mean_precip, y = coef_precip, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_precip - se_precip, ymax = coef_precip + se_precip), width = 8) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. log-log\n(log NPP ~ log MAP)") +
  xlab("MAP")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_MATlog, aes(x = mean_temp, y = coef_mean_temp, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_mean_temp - se_mean_temp, ymax = coef_mean_temp + se_mean_temp), width = 1) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. log-log\n(log NPP ~ log Max Temp)") +
  xlab("MAT")+theme(legend.title = element_blank())


#Draw them lines 

ggplot(Climate_sensitivity_MATlog) +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*coef_precip,
                   yend=range_high_max_precip*coef_precip,
                   color=site_name)) +
  
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. log-log\n(log NPP ~ log MAP)") +
  xlab("MAP")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_MATlog) +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*coef_precip,
                   yend=range_high_max_precip*coef_precip,
                   color=site_name)) +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*(coef_precip-se_precip),
                   yend=range_high_max_precip*(coef_precip-se_precip),
                   color=site_name), linetype="dashed") +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*(coef_precip+se_precip),
                   yend=range_high_max_precip*(coef_precip+se_precip),
                   color=site_name), linetype="dashed") +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("ANPP") +
  xlab("MAP")+theme(legend.title = element_blank())


ggplot(Climate_sensitivity_MATlog) +
  geom_segment(aes(x=range_low_mean_temp,
                   xend=range_high_mean_temp,
                   y=range_low_mean_temp*coef_mean_temp,
                   yend=range_high_mean_temp*coef_mean_temp,
                   color=site_name)) +
  geom_segment(aes(x=range_low_mean_temp,
                   xend=range_high_mean_temp,
                   y=range_low_mean_temp*(coef_mean_temp-se_mean_temp),
                   yend=range_high_mean_temp*(coef_mean_temp-se_mean_temp),
                   color=site_name), linetype="dashed") +
  geom_segment(aes(x=range_low_mean_temp,
                   xend=range_high_mean_temp,
                   y=range_low_mean_temp*(coef_mean_temp+se_mean_temp),
                   yend=range_high_mean_temp*(coef_mean_temp+se_mean_temp),
                   color=site_name), linetype="dashed") +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("ANPP") +
  xlab("MAT")+theme(legend.title = element_blank())


#Log-log----




ggplot(Climate_sensitivity_log, aes(x = mean_precip, y = coef_precip, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_precip - se_precip, ymax = coef_precip + se_precip), width = 8) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. log-log\n(log NPP ~ log MAP)") +
  xlab("MAP")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_log, aes(x = mean_max_temp, y = coef_max_temp, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_max_temp - se_max_temp, ymax = coef_max_temp + se_max_temp), width = 1) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. log-log\n(log NPP ~ log Max Temp)") +
  xlab("Mean max temp")+theme(legend.title = element_blank())


#Draw them lines 

ggplot(Climate_sensitivity_log) +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*coef_precip,
                   yend=range_high_max_precip*coef_precip,
                   color=site_name)) +
  
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. log-log\n(log NPP ~ log MAP)") +
  xlab("MAP")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_log) +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*coef_precip,
                   yend=range_high_max_precip*coef_precip,
                   color=site_name)) +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*(coef_precip-se_precip),
                   yend=range_high_max_precip*(coef_precip-se_precip),
                   color=site_name), linetype="dashed") +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*(coef_precip+se_precip),
                   yend=range_high_max_precip*(coef_precip+se_precip),
                   color=site_name), linetype="dashed") +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("ANPP") +
  xlab("MAP")+theme(legend.title = element_blank())


ggplot(Climate_sensitivity_log) +
  geom_segment(aes(x=range_low_max_temp,
                   xend=range_high_max_temp,
                   y=range_low_max_temp*coef_max_temp,
                   yend=range_high_max_temp*coef_max_temp,
                   color=site_name)) +
  geom_segment(aes(x=range_low_max_temp,
                   xend=range_high_max_temp,
                   y=range_low_max_temp*(coef_max_temp-se_max_temp),
                   yend=range_high_max_temp*(coef_max_temp-se_max_temp),
                   color=site_name), linetype="dashed") +
  geom_segment(aes(x=range_low_max_temp,
                   xend=range_high_max_temp,
                   y=range_low_max_temp*(coef_max_temp+se_max_temp),
                   yend=range_high_max_temp*(coef_max_temp+se_max_temp),
                   color=site_name), linetype="dashed") +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("ANPP") +
  xlab("MAT")+theme(legend.title = element_blank())


#Scaled----


ggplot(Climate_sensitivity_scale, aes(x = mean_precip, y = coef_precip, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_precip - se_precip, ymax = coef_precip + se_precip), width = 8) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. standardized\n(NPP ~ MAP)") +
  xlab("MAP")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_scale, aes(x = mean_max_temp, y = coef_max_temp, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_max_temp - se_max_temp, ymax = coef_max_temp + se_max_temp), width = 1) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. standardized\n(NPP ~ Max Temp)") +
  xlab("Mean max temp")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_scale, aes(x = mean_spei, y = coef_spei, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_spei - se_spei, ymax = coef_spei + se_spei), width = 0.05) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. standardized\n(NPP ~ Max Temp)") +
  xlab("Mean SPEI")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_scale) +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*coef_precip,
                   yend=range_high_max_precip*coef_precip,
                   color=site_name)) +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*(coef_precip-se_precip),
                   yend=range_high_max_precip*(coef_precip-se_precip),
                   color=site_name), linetype="dashed") +
  geom_segment(aes(x=range_low_max_precip,
                   xend=range_high_max_precip,
                   y=range_low_max_precip*(coef_precip+se_precip),
                   yend=range_high_max_precip*(coef_precip+se_precip),
                   color=site_name), linetype="dashed") +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Scaled ANPP") +
  xlab("MAP")+theme(legend.title = element_blank())


ggplot(Climate_sensitivity_scale) +
  geom_segment(aes(x=range_low_max_temp,
                   xend=range_high_max_temp,
                   y=range_low_max_temp*coef_max_temp,
                   yend=range_high_max_temp*coef_max_temp,
                   color=site_name)) +
  geom_segment(aes(x=range_low_max_temp,
                   xend=range_high_max_temp,
                   y=range_low_max_temp*(coef_max_temp-se_max_temp),
                   yend=range_high_max_temp*(coef_max_temp-se_max_temp),
                   color=site_name), linetype="dashed") +
  geom_segment(aes(x=range_low_max_temp,
                   xend=range_high_max_temp,
                   y=range_low_max_temp*(coef_max_temp+se_max_temp),
                   yend=range_high_max_temp*(coef_max_temp+se_max_temp),
                   color=site_name), linetype="dashed") +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Scaled ANPP") +
  xlab("MAT")+theme(legend.title = element_blank())

