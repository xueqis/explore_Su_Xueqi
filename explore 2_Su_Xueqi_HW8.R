
#For your information, the HW8 part starts from line 315

#1.
#A frequency table for every categorical and logical variable

freq_table <- function(data){
  #define a function that accepts a dataframe as the parameter
  lapply(data[,sapply(data,is.logical)],table)
  #apply the lapply() function here to list the frequencies of 
  #each logical variable in a table
  lapply(data[,sapply(data,is.factor)],table)
  #apply the lapply() function here to list the frequencies of 
  #each categorical variable in a table
  
}

data(diamonds)
#call the dataframe diamonds for future use

#check
#freq_table(diamonds)

#2. 
#for numerical variables
#(a)
summary_stat <- function(data){
  #define a function that accepts a dataframe as the parameter
  summary(data[sapply(data, is.numeric)], table)
  #apply the function sapply() to get the numerical variables
  #apply the summary() to get the summary statistics table 
  #for each numerical variables
}

#check
#summary_stat(diamonds)

#(b)
#initiate several variables for future use
rSquare = c()
pairname = c()
num_name = c()
table = c()

num_rsquare <- function(data) {
  #define a function that accepts a dataframe as the parameter
  num_name = colnames(data[sapply(data, is.numeric)])
  #get the names of each numerical variables
  num_data = data[sapply(data, is.numeric)]
  #get numerical data
  numVar = length(num_name)
  #get the total number of numerical variables
  for (i in 1:(numVar-1)){
    for (j in (i+1):numVar) {
      #apply double for loop here to pair any two numerical variables
      pairname = c(pairname, paste(num_name[i], num_name[j], sep = "-"))  #paste any two numerical variables, and separate the names
      #of these two variables by "-"
      rSquare = c(rSquare, paste(summary(lm(num_data[,j]~num_data[,i], num_data))$r.squared))
      #calculate the r-Square values of each pair 
      
    }
  }
  table = data.frame(pairname, rSquare)
  #make a table that has the names of each pair and its associate 
  #r-Square values
  colnames(table) = c("Variable Pairs", "R-Square")
  #name the columns of this table
  return(table)
  #return the table
}
#check
#num_rsquare(diamonds)

#(c)

num_coe <- function(data, threshold = 0.5) {
  #define a function that accepts a dataframe and 
  #a coefficient threshold as parameters.
  #here, we set the default threshold as 0.5
  numcor = c()
  num_name = colnames(data[sapply(data, is.numeric)])
  #get the names of each numerical variables
  num_data = data[sapply(data, is.numeric)]
  #get the numerical data
  numVar = length(num_name)
  #get the number of numerical variables
  for (i in 1:(numVar-1)){
    for (j in (i+1):numVar) {
      #apply the double for loops here to pair any two variables
      #and seperate these two names by "-"
      pairname = c(pairname, paste(num_name[i], num_name[j], sep = "-"))
      numcor = c(numcor, cor(num_data[i], num_data[j], method = "pearson"))
      #calculate the Pearson correlation coefficient of each pair
      
      
    }
    
  }
  pairname = pairname[which(abs(numcor)>threshold)]
  #pick up names of the pairs that its absolute correlation
  #coefficient is bigger than the threshold we set up
  numcor = numcor[abs(numcor)>threshold]
  #pick up the correlation coefficient that its absolute value is 
  #bigger than the threshold value
  table = data.frame(pairname, numcor)
  #make a table that has pairs which satisfy the requirements above 
  colnames(table) = c("Variable Pairs", "Pearson Exceeds Threshold")
  #name the columns of the table
  return(table)
  #return the table
}

#check
#num_coe(diamonds, 0.8)

#3
library(gridExtra)

plot_hist <- function(data, list, binVec = 30){
  #define a function that accepts a dataframe, a list from this data, 
  #and number of bins as parameters
  p1 = ggplot(data, aes(x = list))+geom_histogram(bins = binVec, fill = "blue", col = "black")+geom_vline(aes(xintercept = sapply(list, mean), col = "red"))+theme_grey()+labs(x = colnames(list))
  #plot a histogram of the list
  #use fill to paint the histogram blue
  #use geom_vline() to draw a vertical red line at the mean of 
  #each numerical variables at each	number of	bins integer 
  #specified in	the	bin vector parameter
  #use theme_grey() to set the background color as grey
  #use labs() to name the x-axis
  p2 = ggplot(data, aes(x = list))+geom_histogram(bins = binVec, fill = "blue", aes(y = ..density..))+geom_density()+geom_vline(aes(xintercept = sapply(list, mean), col = "red"))+theme_grey()+labs(x = colnames(list))
  #plot a density histogram of the list
  #use fill to paint the histogram blue
  #use geom_vline() to draw a vertical red line at the mean of 
  #each numerical variables at each	number of	bins integer 
  #specified in	the	bin vector parameter
  #use theme_grey() to set the background color as grey
  #use labs() to name the x-axis
  grid.arrange(p1, p2, nrow = 1, ncol = 2)
  #plot p1 and p2, and arrange these two plots in the same page
}
#check
#plot_hist(num_data, num_data[1])


library(reshape2)
count_grid <- function(data, binVec){
  #define a function that accepts a dataframe, and number of bins 
  #as parameters
  num_data = c()
  num_data = data[sapply(data, is.numeric)]
  #get the numerical data
  num_name = colnames(data[sapply(data, is.numeric)])
  #get the names of numercal datas
  ggplot(melt(num_data), aes(x = value)) + 
    facet_wrap(~ variable, scales = "free", ncol = 2) + 
    geom_histogram(bins = binVec, fill = "blue")+theme_grey()
  #use melt() to plot the histograms(counts) of all 
  #numerical variables in the same page with the number of 
  #bins integer specified in the	bin vector parameter
  #use ncol = 2 inside facet_wrap to indicate that in the same page,
  #each row has two plots
  #use fill to paint the histograms blue
  #use theme_grey() to set the background color as grey
}

#check
#count_grid(diamonds, 20)


density_grid <- function(data, binVec){
  #define a function that accepts a dataframe, and number of bins 
  #as parameters
  num_data = c()
  num_data = data[sapply(data, is.numeric)]
  #get the numerical data
  num_name = colnames(data[sapply(data, is.numeric)])
  #get the names of numercal datas
  ggplot(melt(num_data), aes(x = value)) + 
    facet_wrap(~ variable, scales = "free", ncol = 2) + 
    geom_histogram(aes(y = ..density..), bins = binVec, fill = "blue")+geom_density()+theme_grey()
  #use melt() to plot the histograms(density) of all 
  #numerical variables in the same page with the number of 
  #bins integer specified in the bin vector parameter
  #use ncol = 2 inside facet_wrap to indicate that in the same page,
  #each row has two plots
  #use fill to paint the histograms blue
  #use theme_grey() to set the background color as grey
}

#check
#density_grid(diamonds, 20)



numSwitch <- function (data, switch, binVec){
  #define a function that accepts a dataframe, switch options(coule be
  #'off', 'on', or 'grid'), and the number of bins as 
  #parameters(the number of binVec could be one or more).
  num_data = c()
  num_data = data[sapply(data, is.numeric)]
  #get the numerical data
  num_name = colnames(data[sapply(data, is.numeric)])
  #get the names of numercal datas
  if (switch=="on"){
    #use if/else statement to specify each option of switch
    for (i in 1:length(num_name)){
      for (k in 1:length(binVec)){
        #apply double for loops to plot histogram assigned by 
        #each different bin size
        plot_hist(num_data, num_data[i], binVec[k])
        #call function plot_hist to plot count and density histograms
      }
      
    }
    
  }else if (switch == "grid"){
    cgrid = c()
    dgrid = c()
    for (j in 1:length(binVec)){
      #plot histograms with different bin sizes.
      cgrid[[j]] = count_grid(data, binVec[j])
      #call function count_grid to plot all histograms(count) with 
      #the same assigned bin size in the same page
      dgrid[[j]] = density_grid(data, binVec[j])
      #call function density_grid to plot all histograms(density)
      #with the same assigned bin size in the same page
    }
    print(cgrid)
    print(dgrid)
  }else{
    #if switch == stuff other than "on" and "grid", such as "off"
    return (NULL)
    #return NULL
  }
  
}

#check
#numSwitch(diamonds, "on", c(10, 30)) 


#4

binary_plot <- function(data, list){
  #define a function that acceptc a dataframe and a list from this data
  #as parameters
  bplot = ggplot(data, aes(x = list))+geom_bar(fill = "gray")+theme_bw()+labs(x = names(list))
  #plot a grey bar graph for every categorical and binary variable.
  #use theme_bw() to set the background color as white.
}

binSwitch <- function(data, switch){
  #define a function that accepts a dataframe and switch options
  #(coule be 'off', 'on', or 'grid') as parameters
  bin_data = c()
  bin_data1 = data[sapply(data, is.factor)]
  #get the data of categorical variables
  bin_data2 = data[sapply(data, is.logical)]
  #get the data of binary variables 
  bin_data = data.frame(bin_data1, bin_data2)
  #combine the categorical and binary variables into one dateframe
  bin_name = colnames(bin_data)
  #get the colnames of the bin_data
  binplot = c()
  
  if(switch == "on"|switch == "grid"){
    #use if/else statement to specify each options of switch 
    for (i in 1:length(bin_name)){
      
      binplot[[i]] <- binary_plot(bin_data, bin_data[,i])+labs(x = bin_name[i])
      #plot a grey bar graph for each categorical and 
      #binary variables, and name the x-axis of each bar graph
      
    }
    print(binplot)
    #print the binplot
    
  }else{
    #if switch == stuff other than "on" and "grid", such as "off"
    return (NULL)
    #return NULL
  }
  
}

#check 
#binSwitch(diamonds, "off")


#finally
explore <- function(data, threshold = 0.5, switch = 'off', 
                    binVec = 30){
  #define a function that accepts four variables as parameters, 
  #which are (1). a dataframe, (2). a plot switch	that can accept	
  #three values: off,	on,	or grid, (3). a threshold cut-off value 
  #between 0 and 1 for correlations, and (4). an optional 
  #vector	that contains	one	or more	integers that	represent	
  #the	numbers	of bins	to use for a histogram.	If the vector	
  #is not	provided,	then let ggplot use its default
  
  mylist <- list(freq_table(data), summary_stat(data),
                 num_rsquare(data), num_coe(data, threshold))
  #make a list that call four function we construct above
  numSwitch(data, switch, binVec)
  #call function numSwitch to plot histograms for numerical variables
  binSwitch(data, switch)
  #call function numSwitch to plot histograms for categorical and
  #binary variables
  print(mylist)
  #print mylist
}

#check
#explore(diamonds, 0.9, "grid", c(10, 50))


#HW8 
#Groups C&D
#1
explore2 <- function(data, threshold=0.5, switch="off", binVec=30){
  #define a undated version of function explore that still
  #accepts four variables as parameters, which are 
  #(1). a dataframe, (2). a plot switch	that can accept	
  #three values: off,	on,	or grid, (3). a threshold cut-off value 
  #between 0 and 1 for correlations, and (4). an optional 
  #vector	that contains	one	or more	integers that	represent	
  #the	numbers	of bins	to use for a histogram.	If the vector	
  #is not	provided,	then let ggplot use its default
  #However, 
  #This function will consider and gracefully handle exceptions
  #of function explore (for example, what if the first parameter 
  #is not a dataframe)
  
  data <- na.omit(data)
  #omit the whole row if there are any nas
  #if the first parameter is not a dataframe, change it to
  #a dataframe
  if(!is.data.frame(data)){                 
    data <- as.data.frame(data)
  }
  
  #if the switch option is not one of "off", "on", or "grid", 
  #ask users to re-input it
  while(switch != "off" && switch != "on" && switch != "grid"){  
    print("invalid input for switch")
    #tell the users that the input is not valid
    switch <- readline(prompt="Enter your option(off / on / grid): ")     #ask the users to re-enter the input
  }
  
  
  while(!is.numeric(threshold) || threshold < 0 || threshold >1 ){ 
    
  #if the threshold is not in [0,1], ask users to re-input it
    
    print("correlation threshold must be numeric and in range [0,1]")
    #tell the users the the input is not valid
    threshold <- as.numeric(readline(prompt="Enter your correlation threshold: "))   
    #ask the users to re-enter the input
  }
  
  
  if(!is.null(binVec)){
    if(!is.numeric(binVec)||(is.numeric(binVec) && (TRUE %in% (binVec <= 0)))){ 
    #check if bin vector is all numeric positive, 
    #if the input is not valid, ask the users to re-input it
      print("the bins vector must be numeric and positive vector, please enter new bins one by one and press 'enter' to keep running")
      #tell the users that the input is not valid
      binVec <- c()
      bin <- 1
      #input "enter"  to keep running the loop
      while(bin != ""){ 
        #re-enter the bin vector
        bin <- readline(prompt="Enter the number of bins: ")->bin1
        bin1 <- as.numeric(bin1)
        binVec <- c(binVec, bin1)
      }
      binVec <- na.omit(binVec) 
      #omit the nas
    }
    
    #if the inputed bin vector is not an integer, round it
    if (!is.integer(binVec)) {        
      binVec <- round(binVec)
    }
  }
  return(explore(data,threshold,switch, binVec))

}

#check 
explore2(diamonds,"hello", 1.5)




