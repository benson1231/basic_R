# library -----------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(readr)
library(reshape2)
library(plyr)
library(dplyr)
library(tibble)
library(Rfast)
library(magrittr)

not_all_na <- function(x) any(!is.na(x))
# read file ---------------------------------------------------------------
      setwd("E:/cell viability data/221122 beas2b 72h")
getwd()      
#setwd('/home/nthreeman/R/221030_cy_cellviability/')
path1<-"221122 beas2b 72h.xlsx"

plate_to_df <- function(data=Cr_blk_cv, bygroup=c('agent','cell'), select_sheet = c('c')){
  agent = data$agent[1]
  time = data$time[1]
  name<-paste(agent,time,sep = '_')
  start = which(names(data)%in% '1')
  dose_number = start+length(data$dose)-1
  tb_data <- data[1:3,start:dose_number] 
  # select group
  blank_sum  <- data$blank %>% na.omit() %>% sum() 
  blank_ave  <- blank_sum/length(data$blank %>% na.omit())
  # group
  value <-  tb_data %>% sapply(as.numeric)
  
  get_smallest_variance <- function(df=df,chose_value = chose_value){
    d = list()
    for (i in 1:nrow(df)) {
      t <- combn(df[i,],chose_value)
      s  = min(Rfast::colVars(t))
      d[[i]] = t[,Rfast::colVars(t) == s]
    }
    df = do.call(rbind, d)
    return(df)
  }
  tb_blk <- data.frame(value - blank_ave,check.rows = F)  %>% t() %>%
            get_smallest_variance( chose_value = 3)  # select 3 values with smallest variance 
  rown_tb <- data$dose

  selgroup <- data[bygroup[1]][1,]
  # calculate cell viability
  rownames(tb_blk) <- rown_tb
  
  colnames(tb_blk) <- rep(selgroup,3)  # select 3 values with smallest variance 
  con_tb <- data$control%>% na.omit() %>% mean() 
  tb_blk_cv <- tb_blk/ con_tb  * 100
  return(tb_blk_cv)
}

As_blk_cv <- readxl::read_xlsx(path1,sheet = 'as',col_names = T) %>% dplyr::select(where(not_all_na)) %>% 
              plate_to_df(bygroup = 'agent')
              As_blk.melt <-  reshape2::melt(As_blk_cv)
              colnames(As_blk.melt) <- c('dose','treatment','cell_viability')

Co_blk_cv <- readxl::read_xlsx(path1,sheet = 'co',col_names = T) %>% dplyr::select(where(not_all_na)) %>% 
              plate_to_df(bygroup = 'agent')
              Co_blk.melt <-  reshape2::melt(Co_blk_cv)
              colnames(Co_blk.melt) <- c('dose','treatment','cell_viability')


#===cu
Cd_blk_cv <- readxl::read_xlsx(path1,sheet = 'cd',col_names = T) %>% dplyr::select(where(not_all_na)) %>% 
              plate_to_df(bygroup = 'agent')
              Cd_blk.melt <-  reshape2::melt(Cd_blk_cv)
              colnames(Cd_blk.melt) <- c('dose','treatment','cell_viability')
#===bap
bap_blk_cv <- readxl::read_xlsx(path1,sheet = 'bap',col_names = T) %>% dplyr::select(where(not_all_na)) %>% 
              plate_to_df(bygroup = 'agent')
              bap_blk.melt <-  reshape2::melt(bap_blk_cv)
              colnames(bap_blk.melt) <- c('dose','treatment','cell_viability')



# set up df for plotting --------------------------------------------------

allbind_blk.melt <- rbind(As_blk.melt, Co_blk.melt, 
                         Cd_blk.melt, bap_blk.melt) %>% na.omit()

max = max(allbind_blk.melt$cell_viability)

# plotting in loop --------------------------------------------------------
a<-'dose'
b<-'treatment'
c<-'cell_viability'

library(rstatix) # t-test  Independent sample t-test
stat.test <-allbind_blk.melt %>% 
  group_by(treatment) %>% 
  t_test( cell_viability ~ dose  , p.adjust.method = 'bonferroni') %>%
  filter(p < 0.05) %>% rstatix::add_y_position() %>%
  rstatix::add_significance("p")
  #filter(!p.adj.signif == 'ns')
stat.test_sel <- stat.test %>% subset(., p.adj.signif %in% c( "****","***", "**", "*"))
pvalue_nu = max(summary(stat.test_sel$treatment))
library(stringr)
gp_list <- str_sort(as.character(unique(allbind_blk.melt$treatment)))
my_data <- allbind_blk.melt
my_clone_list <- gp_list
color = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", 'green', 'navy blue')

y_hightest = max * (1+pvalue_nu*0.1)
y_pvalue = max *1.1
for(i in seq_along(my_clone_list)){
  clone = my_clone_list[i]
  p <-ggline(subset(allbind_blk.melt, treatment == clone), y = c, x = a, title = clone, ylim = c(0, y_hightest),
             ylab='cell viability (% of control)', xlab = 'dose (\U00B5M)', legend = "right", 
             color = color[i],
             width = 0.8, add = c("mean_se","dotplot")) + # "dot_plot" or "jitter" or "dotplot"
    theme(legend.title=element_blank()) +
    scale_y_continuous(breaks = seq(0,y_hightest,20),
                       labels = seq(0,y_hightest,20)) +
  
    stat_pvalue_manual(subset(stat.test, treatment == clone), label = "p.adj.signif", tip.length = 0.05, 
                       hide.ns = T,
                       y.position = y_pvalue,
                       step.increase = 0.1,
                       step.group.by = "treatment")
  print(p)
  ggsave(paste(clone,'_',path1,'.png'),width = 12,height = 12,device = 'png',units = 'cm',dpi=300, bg='white')
}

#combination----------------------
library(EBImage)

img_1 = readImage("As _ 221122 beas2b 72h.xlsx .png")
img_2 = readImage("Co _ 221122 beas2b 72h.xlsx .png")
img_3 = readImage("Cd _ 221122 beas2b 72h.xlsx .png")
img_4 = readImage("BaP _ 221122 beas2b 72h.xlsx .png")




png("221122 beas2b 72h_all.png",width = 12,height = 12,res=300, units = 'cm', bg='white')
par(mfrow = c(2, 2))
display(img_1, method = "raster", margin = 1)
display(img_2, method = "raster", margin = 1)
display(img_3, method = "raster", margin = 1)
display(img_4, method = "raster", margin = 1)


dev.off()
