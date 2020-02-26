Coef = c()
Coef2 = c()
Genera = c()
for(i in x:length(taxa_df)){
  Coef[[i]] = tidy(anova(glmer(Binary_dep ~ taxa_df[[i]] + (1 | Random_variable), 
                               data = taxa_df, family = binomial(link = "logit"), 
                               control = glmerControl(optimizer = "bobyqa")), 
                         glmer(Binary_dep ~  (1 | Random_variable), 
                               data = taxa_df, family = binomial(link = "logit"), 
                               control = glmerControl(optimizer = "bobyqa"))
                         ,test = "Chisq"))[2,]
  Coef2[[i]] =  tidy(glmer(Binary_dep ~  taxa_df[[i] + (1 | Random_variable), 
                           data = taxa_df, family = binomial(link = "logit"), 
                           control = glmerControl(optimizer = "bobyqa")))
  Coef2[[i]]$term[6] =  names(taxa_df[i]) # Get taxa names
  Genera[[i]]= Coef2[[i]][6,]
}
#Check

d = ldply(Coef, rbind) %>% mutate(q.value = p.adjust(ldply(Coef, rbind)[,"p.value"],method ="BH"))# Unlist and calculate q value BH
Gen = ldply(Genera, rbind)
GenTab <- cbind(Gen,d)
SG =GenTab %>% arrange(by_group = estimate)
