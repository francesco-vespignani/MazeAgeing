mod8 = lmer(rt ~ lff*rep + (1|item) + (1|id), data=ow1)

out = data.frame()
for (i in levels(ow1$id)) { 
	sb = subset(ow1, id ==i)
	tmp = lmer(rt ~ lff*rep + (1|item), data=sb)
	sm = summary(tmp)
	h =  data.frame(sm$coefficients)
	h$term = c('int','llf','rep','llf:rep')
	h$id = i
	out = rbind(out,h)
}

out$upper = out$Estimate + out$Std..Error
out$lower = out$Estimate - out$Std..Error

a = subset(out, term =='rep')


ggplot() + 
geom_errorbarh(data=a, mapping=aes(y=id, xmin=lower, xmax=upper), height=0.2, size=1, color="blue") + 
geom_point(data=a, mapping=aes(y=id, x=Estimate), size=4, shape=21, fill="white") 



