Coef = c()
Coef2 = c()
Genera = c()
for(i in x:length(y)){
  Coef[[i]] = tidy(anova(glmer(Outcome ~ Treatment +  y[[i]] + (1 | Year)+ (1 | MouseID), 
                               data = y, family = binomial(link = "logit"), 
                               control = glmerControl(optimizer = "bobyqa")), 
                         glmer(Outcome ~ Treatment + (1 | Year) + (1 | MouseID), 
                               data = y, family = binomial(link = "logit"), 
                               control = glmerControl(optimizer = "bobyqa"))
                         ,test = "Chisq"))[2,]
  Coef2[[i]] =  tidy(glmer(Outcome ~  Treatment + y[[i]] + (1 | Year)+ (1 | MouseID), 
                           data = y, family = binomial(link = "logit"), 
                           control = glmerControl(optimizer = "bobyqa")))
  Coef2[[i]]$term[6] =  names(y[i]) # Get taxa names
  Genera[[i]]= Coef2[[i]][6,]
}
#Check

d = ldply(Coef, rbind) %>% mutate(q.value = p.adjust(ldply(Coef, rbind)[,"p.value"],method ="BH"))# Unlist and calculate q value BH
Gen = ldply(Genera, rbind)
GenTab <- cbind(Gen,d)
SG =GenTab %>% arrange(by_group = estimate)
