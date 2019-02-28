# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-02-19
# https://github.com/hrbrmstr/ggalt
# https://github.com/bbc/bbplot

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")
library(tidyverse)
Epidemiology<-phd_field%>% filter(field=="Epidemiology")%>%mutate(year=as.integer(year), n_phds=as.integer(n_phds))
library(bbplot)

#Make plot

tiff("graph4.tiff", units="in", width=10, height=8, res=300)
 ggplot(Epidemiology, aes(x = year, y = n_phds)) +
  geom_line(colour = "#1380A1", size = 1) +
  geom_hline(yintercept = 240, size = 1, colour="#333333") +
  scale_y_continuous(breaks = seq(240, 380, by = 20)) + 
  scale_x_continuous(breaks = seq(2008, 2017, by = 1)) +
  bbc_style() +
  labs(title="PhDs Awarded in Epidemiology",
       subtitle = "PhDs Awarded in Epidemiology in the US. 2008-2017")
dev.off()

library("ggalt")
library("tidyr")

#Prepare data
dumbbell_df <- phd_field%>% 
  filter(major_field=="Biological and biomedical sciences")%>%
  mutate(year=as.integer(year), n_phds=as.integer(n_phds)) %>%
  filter(year == 2008 | year == 2017) %>%
  select(field, year, n_phds) %>%
  spread(year, n_phds) %>%
  mutate(gap = `2017` - `2008`) %>%
  arrange(desc(gap)) %>%
  head(10)
#all of them
dumbbell_df_all <- phd_field%>% 
  filter(major_field=="Biological and biomedical sciences")%>%
  mutate(year=as.integer(year), n_phds=as.integer(n_phds)) %>%
  filter(year == 2008 | year == 2017) %>%
  select(field, year, n_phds) %>%
  spread(year, n_phds) %>%
  mutate(gap = `2017` - `2008`) %>%
  filter(!is.na(gap)) %>% 
  arrange(desc(gap)) 
dumbbell_df_percent<-dumbbell_df_all%>%
  mutate(percent=`2017`/sum(`2017`)) 
#Make plot
tiff("graph1.tiff", units="in", width=16, height=10, res=300)
ggplot(dumbbell_df, aes(x = `2008`, xend = `2017`, y = reorder(field, gap), group = field)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  bbc_style() + 
  labs(title="PhDs Awarded in Biological and Biomedical Sciences",
       subtitle="Biggest Rise of PhDs Awarded, 2008-2017") + 
  geom_text(data=filter(dumbbell_df, field=="Biological and biomedical sciences, general"),
            aes(x=`2008`, y=field, label="2008"),
            color="#FAAB18", size=5, vjust=-2, fontface="bold") + 
  geom_text(data=filter(dumbbell_df, field=="Biological and biomedical sciences, general"),
            aes(x=`2017`, y=field, label="2017"),
            color="#1380A1", size=5, vjust=-2, fontface="bold") + 
  theme(plot.margin=unit(rep(30, 4), "pt"))
dev.off()

tiff("graph2.tiff", units="in", width=20, height=26, res=300)
ggplot(dumbbell_df_all, aes(x = `2008`, xend = `2017`, y = reorder(field, gap), group = field)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  bbc_style() + 
  labs(title="PhDs Awarded in Biological and Biomedical Sciences, 2008-2017") + 
  geom_text(data=filter(dumbbell_df, field=="Biological and biomedical sciences, general"),
            aes(x=`2008`, y=field, label="2008"),
            color="#FAAB18", size=5, vjust=-2, fontface="bold") + 
  geom_text(data=filter(dumbbell_df, field=="Biological and biomedical sciences, general"),
            aes(x=`2017`, y=field, label="2017"),
            color="#1380A1", size=5, vjust=-2, fontface="bold") + 
  theme(plot.margin=unit(rep(30, 4), "pt"))
dev.off()

library(ggplot2)
library(ggalt)
library(scales)
tiff("graph3.tiff", units="in", width=20, height=12, res=300)
ggplot(dumbbell_df_percent, aes(y=reorder(field, percent), x=percent)) + 
  geom_lollipop(point.colour="#1380A1", point.size=3, horizontal=TRUE) + 
  scale_x_continuous(expand=c(0,0), labels=percent,
                     breaks=seq(0, 0.14, by=0.02), limits=c(0, 0.14)) + 
  labs(x=NULL, y=NULL, 
       title="PhDs Awarded in Biological and Biomedical Sciences in 2017"
       # subtitle="Ranked by Field",
       # caption="Data from https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-02-19"
       ) + 
  #theme_minimal(base_family="Arial Narrow") + 
  bbc_style() +
  theme(panel.grid.major.x=element_line(colour = "#dddddd")) + 
  theme(panel.grid.major.y=element_blank()) + 
  theme(panel.grid.minor=element_blank()) + 
  theme(plot.margin=unit(rep(30, 4), "pt")) 
dev.off()

theme_minimal() + 
   + 
  theme(axis.line.y=element_line(color="#2b2b2b", size=0.15)) + 
  theme(axis.text.y=element_text(margin=margin(r=0, l=0))) + 
  theme(plot.margin=unit(rep(30, 4), "pt")) + 
  theme(plot.title=element_text(face="bold",margin=margin(b=10))) + 
  theme(plot.subtitle=element_text(margin=margin(b=10))) + 
  theme(plot.caption=element_text(size=8, margin=margin(t=10)))
