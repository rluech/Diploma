
date <- setdiff(id.day('2017-01-01', 365)$live$label, '2017-07-27')
id <- setup(date, dem.var = 'hhsize', dem.join = TRUE, prg.join = TRUE, import = TRUE)

print(topn=3, na.omit(view)[, .(day,pin,weight,hhsize,chn.name,start,end,dur,act,prg.dur,title,prgtyp,genre)])

id <- setup('2017-01-01', 365, dem.var = 'sg', dem.join = TRUE, prg = FALSE)
import(id)

res <- calc(view)

id$lab$day$live[, week := wday(as.Date(label)) -1][week==0, week := 7]
res[, week := id$lab$day$live[res, on=c(label="day"), week]]

plot(seq_along(res$day), res$dur, pch = 21, bg = ifelse(res$week > 5, 5, 0))

out <- boxplot(dur ~ week, data = res, names = id$lab$day$live[2:8,weekdays(as.Date(label))])
text(1:2, out$out * 1.05, res[.(out$out), on='dur', day]) 

view[, min := start %/% 60]
min <- calc(view, by='min')

plot(dur ~ min, data = min)

id$lab$sta[ttv==1 & nchar(chn.type)>1, .N, k=.(chn.type, chn.name)]
id$lab$genre
