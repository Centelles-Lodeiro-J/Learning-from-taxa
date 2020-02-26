library(lme4)
library(broom)
library(tidyverse)
library(plyr)

Coef = c()
Coef2 = c()
Markers = c()


for(i in x:length(data_marker)){
  Coef[[i]] = tidy(anova(lmer(continous_dep[[i]] ~ fixed_variable + (1 | Random_variable), 
                              data = data_marker),
                         lmer(continous_dep[[i]] ~  (1 | Comp.groups), 
                              data = data_marker)
                         ,test = "Chisq"))[2,]
  Coef2[[i]] =  tidy(lmer(continous_dep[[i]] ~ fixed_variable+ (1 | Random_variable), 
                          data = data_marker))
  Coef2[[i]]$term[1] =  names(data_marker[i]) # Get taxa names
  Markers[[i]]= Coef2[[i]][1,]
}
d = ldply(Coef, rbind) %>% mutate(q.value = p.adjust(ldply(Coef, rbind)[,"p.value"],method ="BH"))# Unlist and calculate q value BH
Gen = ldply(Genera, rbind)
GenTab <- cbind(Gen,d)
SG =GenTab %>% arrange(by_group = estimate)
