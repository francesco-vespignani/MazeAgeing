
aat = subset(reg, cond %in% c('AAT'))
aat$item = factor(as.numeric(as.character(aat$item)), ordered=TRUE)
l = lmer(lrt~rep*item + (1|scode), data = aat)
plot_model(l, type='pred', terms=c('item', 'rep'))

l = lmer(lrt~rep*item*lff*nchar + (1|scode), data = aat)
plot_model(l, type='pred', terms=c('item', 'rep'))


aat$item  = as.numeric(aat$item)
l = lmer(lrt~rep*item + (1|scode), data = aat)
plot_model(l, type='pred', terms=c('item', 'rep'))


sand = subset(reg, cond %in% c('SAND'))
sand$item = factor(as.numeric(as.character(sand$item))-20, ordered=TRUE)
l = lmer(lrt~rep*item + (1|scode), data = sand)
plot_model(l, type='pred', terms=c('item', 'rep'))

l = lmer(lrt~rep*item*lff*nchar + (1|scode), data = sand)
plot_model(l, type='pred', terms=c('item', 'rep'))

sand$item  = as.numeric(sand$item)
l = lmer(lrt~rep*item + (1|scode), data = sand)
plot_model(l, type='pred', terms=c('item', 'rep'))


