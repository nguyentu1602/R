# File to match the ES and TY datasets
start <- index(ESmatch)[1]
end <-index(TYmatch)[430067] 
period <- paste(start,end,sep='/')

# Merge. The .1 names belong to the TY series
ESTYmatch <- merge.xts(ESmatch[period],TYmatch[period])
ESTYmatch.quartersecond <- merge.xts(ESmatch.quartersecond[period],TYmatch.quartersecond[period])
ESTYmatch.halfsecond <- merge.xts(ESmatch.halfsecond[period],TYmatch.halfsecond[period])
ESTYmatch.second <- merge.xts(ESmatch.second[period],TYmatch.second[period])
ESTYmatch.2second <- merge.xts(ESmatch.2second[period],TYmatch.2second[period])
ESTYmatch.5second <- merge.xts(ESmatch.5second[period],TYmatch.5second[period])
ESTYmatch.10second <- merge.xts(ESmatch.10second[period],TYmatch.10second[period])
ESTYmatch.20second <- merge.xts(ESmatch.20second[period],TYmatch.20second[period])
ESTYmatch.30second <- merge.xts(ESmatch.30second[period],TYmatch.30second[period])
ESTYmatch.minute <- merge.xts(ESmatch.minute[period],TYmatch.minute[period])
ESTYmatch.5minute <- merge.xts(ESmatch.5minute[period],TYmatch.5minute[period])

namelist <- ls(pattern="match")
namelist
save(list=namelist, file="~/Desktop/ecn395data/ESTY_match_files_all.R")




