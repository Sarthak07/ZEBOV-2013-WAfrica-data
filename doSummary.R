require("xts")

	# a function to make an array of Administrative Area cases
	summarizeAA <- function(level,ISO3,country){

		cat(paste("summarizing",country,"with",ISO3,"at level",level),fill=TRUE)

		i <- read.delim("SEIRDh_data_flat.txt",sep = "\t",fill=TRUE,header=TRUE,stringsAsFactors=FALSE)
		i <- i[i$level==level&i$country==ISO3,]

		# Make a list of Administrative Area Names
		aa <- unique(i$area)

		AA <- NULL

		AA <- loadZEBOVxts(0,country,0)$tot_case

		#iterator for on a for AAs  
		for(a in seq_along(aa)){
			cat(paste("adding ",aa[a],"at level",level),fill=TRUE)
			AA  <- merge.xts(AA,loadZEBOVxts(level,aa[a],0)$tot_case)
		}

		# Throw the list to column headers
		# will this work?
		colnames(AA) <- c('Total',aa)

		tdiff <- as.numeric(first(index(AA))-as.Date("2013-12-01"))

		cat(tdiff,fill=TRUE)
		# get day count
		t <- as.numeric(index(AA))+tdiff
		t0 <- as.numeric(first(index(AA)))
		AA$day <- t-t0
		
		#drop NAs
		AA <- AA[!is.na(AA$Total),]
		return(AA)

	}


	summarize <- function(){
		SLE <- loadZEBOVxts(0,"SierraLeone",0)
		LBR <- loadZEBOVxts(0,"Liberia",0)
		GIN <- loadZEBOVxts(0,"Guinea",0)
		all <- GIN+SLE+LBR

		region<-NULL
		region<- all
		region$house <- NULL
		region$tot_recov <- NULL
		region$cur_admit <- NULL
		region$GINc <- GIN$tot_case
		region$GINd <- GIN$tot_death
		region$SLEc <- SLE$tot_case
		region$SLEd <- SLE$tot_death
		region$LBRc <- LBR$tot_case
		region$LBRd <- LBR$tot_death

		# get day count
		t <- as.numeric(index(region))
		t0 <- as.numeric(first(index(region)))-1

		region$day <- t-t0

		return(region)

	}


	#
	# regularize an irregular xts
	#
	# takes an irregular xts
	# returns a regular xts
	# 
	# This is probably not the most robust thing in the world, and might break if given real Z-ordered data
	#
	# usage x <- regular(x.zoo)
	#

	regular <- function(x,extend=365){

		# make all the days
		empty <- seq.Date(from=index(first(x)), to=index(last(x))+extend, by="day")
		e.xts <- as.xts(empty)

		# get day count
		t <- as.numeric(index(e.xts))
		t0 <- as.numeric(first(index(e.xts)))-1
		e.xts$day <- t-t0


		reg.x <- merge(e.xts,x)

	#            return(reg.x) 


		#interp and trunc NAs
		#x.xts<-na.approx(reg.x)
		#x.xts <- round(x.xts.float)
	
	     return(reg.x)
	}


	# example, load Sierra Leone loadZEBOVxts(1,"SierraLeone")
	# example, load Monrovia loadZEBOVxts(3,"Monteserrado")

	loadZEBOVxts <- function(level=1,area,extend=365){

		i <- read.delim("SEIRDh_data_flat.txt",sep = "\t",fill=TRUE,header=TRUE)
		i <- i[i$level==level&i$area==area,]		
		i$area <- NULL
		i$source <- NULL
		i$country <- NULL

		i.zoo <- read.zoo(i,format = '%Y%m%d', tz='')
		index(i.zoo) <- as.Date(index(i.zoo),format = '%Y-%m-%d')
		i.xts <- as.xts(cbind(i.zoo$tot_case, i.zoo$cur_admit, i.zoo$tot_recover, i.zoo$tot_death , i.zoo$house))
		colnames(i.xts) <- c("tot_case","cur_admit","tot_recov","tot_death","house")
                reg.xts<-regular(i.xts,extend)

	  return(reg.xts)
	}

s <- summarize()
write.csv(s,"summary.csv",row.names=as.Date(index(s)))

gin <- summarizeAA(2,'GIN',"Guinea")
write.csv(gin,"summary.GIN.csv",row.names=as.Date(index(gin)),na = "")

sle <- summarizeAA(2,'SLE',"SierraLeone")
write.csv(sle,"summary.SLE.csv",row.names=as.Date(index(sle)),na = "")

lbr <- summarizeAA(1,'LBR',"Liberia")
write.csv(lbr,"summary.LBR.csv",row.names=as.Date(index(lbr)),na = "")
