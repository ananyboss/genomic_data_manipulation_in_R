
inp=read.table("Problem II - transform.txt")                                                   #Taking input

no_of_col=ncol(inp)/3                                                                          #calculating no. of columns in output file
no_of_row=nrow(inp)                                                                            #calculating no. of rows in output file

df = data.frame(matrix(NA, ncol=no_of_col, nrow=no_of_row))[-1]                                #making an empty data frame of calculated size


for(i in 1:dim(df)[1]){                                                                                                                               
	df[i,1]=as.character(inp[i,1])                                                         #first column is as it is

	for(j in 1:(dim(df)[2]-1)){                                                            #this loop chooses sets of three one by one
		if(inp[i,3*j+1]==1)                                                            #if first column in chosen set of three columns is 1
			df[i,j+1]=paste(as.character(inp[i,2]),as.character(inp[i,2]),sep="")  #fulfill conditions and place answer in data frame

		if(inp[i,3*j+2]==1)
			df[i,j+1]=paste(as.character(inp[i,2]),as.character(inp[i,3]),sep="")

		if(inp[i,3*j+3]==1)
			df[i,j+1]=paste(as.character(inp[i,3]),as.character(inp[i,3]),sep="")
	}
}


print(df)                                                                                      #print data to console


