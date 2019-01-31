
# generate predictors for hh composition based on viewing behavior

# - Exclude Guest and Guestviewing. To match reality keep guestviewing 
# - use unweighted viewing
# - add sg
# - Choose 8 recent Weeks with lots of viewing and typical viewing: 
#   -> In Automn, no holidays/festivals, no special TV events (Sportevents)
# - define the sample on a specific day in the middle of that period
#   hhsize number of individuals, sex and particularly age may chage from day to day

# --- global ------------------------------------------------------------------

library('tv')
demday <- as.Date('2017-11-12')
days <- as.character(seq(demday-28, demday+27, by = "day")) 
# table(weekdays(as.Date(days)))

# --- understand hhsize -------------------------------------------------------

id <- setup(days, dem.var = c("hhsize","age","sex"), dem.uni = FALSE )
import(id)

dem[, hh := as.integer(pin/100)]
ordercol(dem, "hh", "day")

# hhsize is NOT constant over time. Because people move in or move out.
id.hh <- dem[, uniqueN(hhsize) == 1, k = hh]
dem[id.hh[(!V1)], on="hh"][, .(hh,day,hhsize,pin)]

# hhsize is NOT necessarily equal to the sum of individuals (ignoring guests)
# 1. Babys 0-2 years old are excluded, this explains hhsize > sum(individuals)
# 2. The variable hhsize has levels 1,2,..,5+, more than 5 hh members are 
#    labeled "5+", this explains hhsize < sum(individuals)
# 
id.hh <- dem[(!guest), .(hhsize = hhsize, n.ind = uniqueN(pin)), k=.(day,hh)] 
id.hh[, dif :=  n.ind - hhsize]
id.hh <- id.hh[, .N, k=.(hh,hhsize,n.ind,dif)][!duplicated(hh)][,-"N"]
table(id.hh[, .(hhsize, dif)]) # distribution seems plausible

# --- hh composition ----------------------------------------------------------

id <- setup(days, obs = "ind", dem.var = c("sg","hhsize","age","sex"), 
            dem.day = demday, dem.uni= FALSE, view = FALSE, prg = FALSE)
import(id)

dem[, `:=` (hh = pin %/% 1e2L, ind = pin %% 1e2L)]
ordercol(dem, c("hh","ind"), "day")
dem <- dem[(!guest)] # excluding 117 guests
# dem[, .N, k=.(day,hh,sg,hhsize)][, .N, k=hh][N>1] # must be empty
hh <- dcast(dem, day + hh + sg + hhsize ~ ind, value.var = c("age","sex"), fill = 0L)
setnames(hh, 'hh', 'pin')
rm(id, dem)

# --- import data -------------------------------------------------------------

id <- setup(
  day = days,
  guest = FALSE,
  obs = "hh",
  dem.var = "sg",
  # dem.day = demday, # wir brauchen dem aus allen Tagen um Ansesenheit miteinzubeziehen
  tmb = list(
    '02to06' = c(start = '02:00:00', end = '05:59:59'),
    '06to08' = c(start = '06:00:00', end = '07:59:59'),
    '08to11' = c(start = '08:00:00', end = '10:59:59'),
    '11to13' = c(start = '11:00:00', end = '12:59:59'),
    '13to17' = c(start = '13:00:00', end = '16:59:59'),
    '17to20' = c(start = '17:00:00', end = '19:59:59'),
    '20to22' = c(start = '20:00:00', end = '21:59:59'),
    '22to24' = c(start = '22:00:00', end = '23:59:59'),
    '24to02' = c(start = '24:00:00', end = '25:59:59')
  )
  )
import(id)

# --- sum viewing by weekpart and daytime -------------------------------------

# the mean over several days is not trivial. For every pin N days can change.
# N days have to be calculated in dem similar like calc.uni().
# But if you average over weekends and workdays, you have to find N days for each
# of that day-grouping for each pin. see nday below:

dem.add(dem, 'calendar')
nday <- dem[, .(N = uniqueN(day)), k=.(pin, wend)]

calc.uni(dem[, weight := 1L]) # ungewichtet

res <- calc(
  dt = view[dem, on=c('day',"pin")], 
  by = c("day","tmb","pin"), 
  period = "tmb.dur"
)

dem.add(res, 'calendar')

res <- res[, sum(dur), k=c("wend","tmb","pin")]
res[, nday := nday[res, on=c('wend','pin'), N]]
# na.omit(res, invert = TRUE)
res[, mean.dur := V1 / nday]
res[, wend := id$lab$wend[res, on='wend', label]]

X.tmb <- dcast(res, pin ~ wend + tmb, value.var = "mean.dur")
X.tmb <- X.tmb[hh[,.(pin)], on="pin"]
na.to.0(X.tmb) # na.omit(X.tmb, invert = TRUE)
setnames(X.tmb, -1, paste0("day_",tolower(names(X.tmb)[-1])))

# --- sum viewing by channel groups ----------------------------------------

# have a look at channel groups made by Luca
# x <- id$lab$sta[ttv==1 & id < 8000]
# lapply(split(x$chn.name, x$chn.type), unique)

# cols <- c("chn.type","chn.country","chn.lang")
# id.virt <- id$lab$sta[ttv==1 & id < 8000, .N, k=c("chn","chn.name",cols)][,-"N"]
# id.virt[, .N, k=chn.name][N>1] 
# "chn.type","chn.country" ist unique auch auf Sender Ebene, "chn.lang" aber nicht
# => verwende erst mal Streams

cols <- c("chn.type","chn.country","chn.lang")
view[, (cols) := id$lab$sta[view, on=c(id="base"), mget(cols)]]
ordercol(view, cols, "chn.name")
# view[, any(is.na(chn.type))]

res.type <- calc(
  dt = view[dem, on=c('day',"pin")], 
  by = c("day","pin","chn.type"), 
  period = "day"
)

res.type <- res.type[, sum(dur), k=c("chn.type","pin")]
res.type[, nday := nday[, .(N=sum(N)), k=pin][res.type, on='pin', N]]
# na.omit(res.type, invert = TRUE)
res.type[, mean.dur := V1 / nday]

X.chn.type <- dcast(res.type, pin ~ chn.type, value.var = "mean.dur")
X.chn.type <- X.chn.type[hh[,.(pin)], on="pin"]
na.to.0(X.chn.type) # na.omit(X.chn.type, invert = TRUE)



res.country <- calc(
  dt = view[dem, on=c("day","pin")], 
  by = c("day","pin","chn.country"), 
  period = "day"
)
res.country <- res.country[, sum(dur), k=c("chn.country","pin")]
res.country[, nday := nday[, .(N=sum(N)), k=pin][res.country, on='pin', N]]
# na.omit(res.country, invert = TRUE)
res.country[, mean.dur := V1 / nday]

X.chn.country <- dcast(res.country, pin ~ chn.country, value.var = "mean.dur")
X.chn.country <- X.chn.country[hh[,.(pin)], on="pin"]
na.to.0(X.chn.country) # na.omit(X.chn.country, invert = TRUE)



res.lang <- calc(
  dt = view[dem, on=c("day","pin")], 
  by = c("day","pin","chn.lang"), 
  period = "day"
)
res.lang <- res.lang[, sum(dur), k=c("chn.lang","pin")]
res.lang[, nday := nday[, .(N=sum(N)), k=pin][res.lang, on='pin', N]]
# na.omit(res.lang, invert = TRUE)
res.lang[, mean.dur := V1 / nday]

X.chn.lang <- dcast(res.lang, pin ~ chn.lang, value.var = "mean.dur")
X.chn.lang <- X.chn.lang[hh[,.(pin)], on="pin"]
na.to.0(X.chn.lang) # na.omit(X.chn.lang, invert = TRUE)

X.chn <- X.chn.type[,-"0"][X.chn.country[,-"0"][X.chn.lang[,-"0"], on="pin"], on="pin"]
setnames(X.chn, -1, paste0("chn_",tolower(names(X.chn)[-1])))

# --- genre -------------------------------------------------------------------

# Holger: Genre is somewhat unreliable

view <- overlap.join(view, prog, type='prg')

res.genre <- calc(
  dt = view[dem, on=c("day","pin")], 
  by = c("day","pin","genre"), 
  period = "day"
)
res.genre <- res.genre[, sum(dur), k=c("genre","pin")]
res.genre[, nday := nday[, .(N=sum(N)), k=pin][res.genre, on='pin', N]]
# na.omit(res.genre, invert = TRUE)
res.genre[, mean.dur := V1 / nday]

X.genre <- dcast(res.genre, pin ~ genre, value.var = "mean.dur")
X.genre <- X.genre[hh[,.(pin)], on="pin"]
na.to.0(X.genre) # na.omit(X.genre, invert = TRUE)
setnames(X.genre, -1, paste0("prg_",tolower(names(X.genre)[-1])))

# --- out ---------------------------------------------------------------------

setorder(hh[, day := NULL], "pin")
ordercol(hh, 'pin')
hh.composition <- as.data.frame(hh)

predictors <- X.tmb[X.chn[X.genre, on="pin"], on="pin"]
predictors <- as.data.frame(predictors)

setnames(hh.composition, 'pin', 'hh')
setnames(predictors, 'pin', 'hh')

setorder(hh.composition, 'hh')
setorder(predictors, 'hh')

save(hh.composition, predictors, file = '~/diplom/data/data_predictors.RData')

