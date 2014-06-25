set.seed(7212014)
library(plyr)
library(ggplot2)

### R CONTRIBUTORS ###
CONNECT <- url("http://www.r-project.org/contributors.html")
contrib <- readLines(con=CONNECT,encoding="latin1",warn=FALSE) # Download
close(CONNECT)

contrib <- paste(contrib,collapse=" ")
NAME <- "(.)([A-Z][a-z]+ [van |de ]?[A-Z][a-z']+)(.)"
SUBNAME <- "(.*)([A-Z][a-z]+ [van |de ]?[A-Z][a-z']+)(.*)"
contrib.split <- strsplit(gsub(NAME,"*\\2*",contrib),split="*",fixed=TRUE)[[1]] # Separate name patterns
names <- sub(SUBNAME,"\\2",contrib.split[grep("([A-Z][a-z]+ [van |de ]?[A-Z][a-z']+)",contrib.split)]) # Extract names
names <- names[names!="Project Contributors"&names!="Statistics Department"] # Clean-up
names <- unique(names)
first.names <- sub("([A-Z][a-z]+)( .*)","\\1",names)

# genderize
BASEURL <- "http://api.genderize.io/?"

name.string <- paste("name[#]=",first.names,collapse="&",sep="")
for(i in 0:(length(first.names)-1))
	name.string <- sub("#",i,name.string,fixed=TRUE)

CONNECT <- url(paste(BASEURL,name.string,sep="",collapse=""))
CoreGender <- readLines(con=CONNECT,warn=FALSE)
close(CONNECT)

ProcessGender <- function(str){
	
	x <- strsplit(str,split=":")[[1]]
	
	gender.index <- x[grep("male",x)]
	prob.index <- x[grep("[0|1]\\.[0-9][0-9]",x)]
	
	gender <- rep("male", length(gender.index))
	if(length(grep("female",gender.index))>0)
		gender[grep("female",gender.index)] <- "female"
	
	prob <- sub("(.*)([0|1]\\.[0-9][0-9])(.*)","\\2",prob.index)

data.frame(
	gender = gender,
	prob = prob
	)
}

### CONTRIBUTED PACKAGES ###

CONNECT <- url("http://cran.r-project.org/web/packages/available_packages_by_name.html")
pkg <- readLines(con=CONNECT)
close(CONNECT)

PKGPATTERN <- "(.*packages/)([A-Za-z][A-Za-z0-9]+)(/index\\.html.*)"
pkgs <- sub(PKGPATTERN,"\\2",pkg[grep(PKGPATTERN,pkg)])
n.sample <- floor(.10*length(pkgs)) # RANDOM 10%
pkg.sample <- sample(pkgs, n.sample)

PKGURL <- "http://cran.r-project.org/web/packages/NAME/index.html"

pkg.authors <- sapply(pkg.sample, function(x){
	CONNECT <- url(sub("NAME",x,PKGURL))
	result <- readLines(con=CONNECT, warn=FALSE)
	close(CONNECT)
	author.index <- grep("Author",result)
	name <- sub("(<td>)(.*)","\\2",result[author.index+1]) # Take first name occurrence	
	name <- sub("\\.","",name) # Remove any initials	
	name <- strsplit(name,split=" ")[[1]]
	nc <- sapply(name,nchar)
	name <- name[nc!=1][1]
sub("[[:punct:]]","",name)
})

pkg.authors <- pkg.authors[-grep("[[:punct:]]",pkg.authors)] # Remove a few irregular entries

# Genderize; Split due to genderize.io limits
name.string <- paste("name[#]=",pkg.authors[1:301],collapse="&",sep="") ## NEED TO SPLIT
for(i in 0:300)
	name.string <- sub("#",i,name.string,fixed=TRUE)

CONNECT <- url(paste(BASEURL,name.string,sep="",collapse=""))
PkgGender1 <- readLines(con=CONNECT,warn=FALSE)
close(CONNECT)

name.string <- paste("name[#]=",pkg.authors[302:length(pkg.authors)],collapse="&",sep="") ## NEED TO SPLIT
for(i in 0:(length(pkg.authors)-1))
	name.string <- sub("#",i,name.string,fixed=TRUE)

CONNECT <- url(paste(BASEURL,name.string,sep="",collapse=""))
PkgGender2 <- readLines(con=CONNECT,warn=FALSE)
close(CONNECT)

## Gender Results
CoreGender <- ProcessGender(CoreGender)
CoreGender$prob <- as.numeric(levels(CoreGender$prob)[CoreGender$prob])
CoreGender$prob <- CoreGender$prob/sum(CoreGender$prob)

PkgGender <- rbind(ProcessGender(PkgGender1), ProcessGender(PkgGender2))
PkgGender$prob <- as.numeric(levels(PkgGender$prob)[PkgGender$prob])
PkgGender$prob <- PkgGender$prob/sum(PkgGender$prob)


CoreSummary <- ddply(CoreGender, "gender", summarize,
	prop = sum(prob)
)

PkgSummary <- ddply(PkgGender, "gender", summarize,
	prop = sum(prob)
)

TotalSummary <- rbind(CoreSummary, PkgSummary)
TotalSummary$Type <- rep(c("Core Contributors","Package Authors"),each=2)
TotalSummary$Label <- paste(round(TotalSummary$prop*100,1),"%",sep="")

cbPalette <- c("#E69F00", "#56B4E9")

ggplot(TotalSummary, aes(x=gender,y=prop,fill=gender))+
	scale_fill_manual(values=cbPalette)+
	geom_bar(stat="identity")+
	facet_grid(~Type)+
	theme(legend.position="none",text=element_text(size=12))+
	scale_y_continuous("Proportion")+
	scale_x_discrete("")+
	geom_text(aes(label=Label),vjust=0)
		
	