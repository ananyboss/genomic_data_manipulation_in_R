library(ggplot2)                                                                                 #Loading library to help perform plotting 
inp=read.table("Problem I - rsids.txt")                                                          #Taking input


inp <-within(inp, V4<-data.frame(do.call('rbind', strsplit(as.character(V4), '|', fixed=TRUE)))) #Splitting the last column at the '|' symbol

for(i in 1:dim(inp)[1]){                                                                         #This for loop removes wrong entries by checking is "rs" is present in the rsID column or not
	if(is.null(grep('rs',inp[i,1])) || length(grep('rs',inp[i,1])) != 1 || is.na(grep('rs',inp[i,1])) || !grep('rs',inp[i,1])){
		inp <- inp[-c(i), ]                                                              #Removes row no. i from the data frame
	}
}



for(i in 1:(dim(inp)[1]-1)){
	for(j in (i+1):dim(inp)[1]){
		if(identical(inp[i,1], inp[j,1])){                                               #Checks whether rsID are same or not


			if(is.null(as.numeric(inp$V4[i,2])>as.numeric(inp$V4[j,2]))){            #Branching on basis of call rate
       
				de <- c(as.character(inp[j,1]),as.character(inp[j,2]),as.character(inp[j,3]),as.character(inp$V4[j,1]),as.character(inp$V4[j,2]))                                                                #conversion of row entry to a vector
     
				entry=paste(de,collapse=",")                                     #Changing it to csv format
				write(entry,file="summary.csv",append=TRUE)                      #writing it to summary.csv
				inp <- inp[-c(j), ]                                              #Removes row no. j from the data frame
			}



			else{
        
				de <- list(as.character(inp[i,1]),as.character(inp[i,2]),as.character(inp[i,3]),as.character(inp$V4[i,1]),as.character(inp$V4[i,2]))
  
				entry=paste(de,collapse=",")
				write(entry,file="summary.csv",append=TRUE)
				inp <- inp[-c(i), ]


			}
		}
	}
}



p1 <- ggplot(data=inp,aes(x=V2,y=V4$X2,col=V4$X1))+ylab("call rate")+xlab("position")            #Plotting graph of call rate vs position
p1 + geom_point()+theme(axis.text.y=element_blank())


inp$V4=paste(inp$V4$X1,"|",inp$V4$X2)                                                            #Recombining last two columns

print(inp)                                                                                       #printing final data on console

