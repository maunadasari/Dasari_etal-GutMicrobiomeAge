#Reviewer Comments
library(tidyverse)

#Review: Need to add percentages of female and male samples before vs. after first birth/male dispersal & male testicular enlargement. If it seems biased, add a caveat.

metadata<-readRDS("data/metadata_bstatus0_mathormonerank_rainpriortomilestone_matrankbirthgroup.rds") 

short_meta<-metadata %>%  
  select(DADA_id, sname, sex, age.years)

matured<-short_meta%>% 
  mutate(matured=case_when(sex=="F"&age.years>=4.51~1,
                           sex=="M"&age.years>=5.41~1,
                           sex=="F"&age.years<4.51~0,
                           sex=="M"&age.years<5.41~0),
         reproductive=case_when(sex=="F"&age.years>=5.97~1,
                                sex=="M"&age.years>=7.43~1,
                                sex=="F"&age.years<5.97~0,
                                sex=="M"&age.years<7.43~0),
         ranked=case_when(sex=="F"&age.years>=2.5~1,
                          sex=="M"&age.years>=7.38~1,
                          sex=="F"&age.years<2.5~0,
                          sex=="M"&age.years<7.38~0),
         old=case_when(age.years>=7.38~1,
                       age.years<7.38~0)) %>% 
  group_by(sex,matured,ranked,reproductive,old) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate(percent=count/sum(count)*100)

sum(matured$count)

#Review: Do long-lived individuals experience higher diversity throughout life, compared to shorter-lived individuals?

##Approach: check how many individuals are extremely long-lived
old<-short_meta %>% 
  filter(age.years>=20) %>% 
  group_by(sname) %>% 
  summarise(count=n()) %>% 
  ungroup()

sum(old$count) #256 samples, 18 individuals, all female

#Review: I also agree with the authors that cause of death can be completely unrelated to the microbiome, but I would expect a potential with early life growth trajectories (i.e. age at maturity). I wonder if this could interact with something like season of birth. I may be splitting variables too much now given the caveats put forth by the authors already, but it is interesting to consider. Again, I do not think these issues affect the detected relationships, but it might influence how the authors present the microbiome clock in the introduction and/or discussion. 

## Approach: show how few samples are available for the example ea
ea<-readRDS("data/ea_dataset.rds")

drought<-ea %>% 
  filter(drought==T)

menarche<-readRDS("data/menarche_lifespan_linear.rds") %>% 
  filter(sname %in%drought$sname)

