
item_ID = c(1,2,3,4,5,6,7,8,9,10)
age_40 = c('T','T','T','T','T','F','F','F','F','F')
edu = c('hs','ma','under','under','under','ma',
        'hs','ma','ma','hs')
occup = c('se','pg','l','pg','se','se',
          'pg','l','pg','se')
loan = c(1, 0 , 1 , 0 , 1 , 0 , 0 , 0 ,
         0,0)
data = data.frame(cbind(item_ID,age_40,
                        edu,occup,loan))

data_plot = data.frame(cbind(item_ID,as.factor(age_40),
                             as.factor(edu),as.factor(occup),loan))

#compute Shannon entropy
entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}

entropy(data$loan)

library(tidyverse)
#returns IG for numerical variables.
IG_numeric<-function(data, feature, target) {
  #Strip out rows where feature is NA
  data<-data[!is.na(data[,feature]),]
  #compute entropy for the parent
  e0<-entropy(data[,target])

  data$cat<- as.factor(data[,feature])
  #use dplyr to compute e and p for each value of the feature
  dd_data <- data %>% group_by(cat) %>% summarise(e=entropy(get(target)), 
                                                  n=length(get(target))
  )
  
  #calculate p for each value of feature
  dd_data$p<-dd_data$n/nrow(data)
  #compute IG
  IG<-e0-sum(dd_data$p*dd_data$e)
  
  return(IG)
}



IG_numeric(data,"age_40","loan")
IG_numeric(data,"edu","loan")
IG_numeric(data,"occup","loan")




table(data$age_40,data$loan)
table(data$edu,data$loan)
table(data$occup,data$loan)

gini_process <-function(classes,splitvar = NULL){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- 1-sum(crossprob[,1]**2)
  Yes_Node_Gini <- 1-sum(crossprob[,2]**2)
  return(sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
}

gini_process3 <-function(classes,splitvar = NULL){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- 1-sum(crossprob[,1]**2)
  No2_Node_Gini <- 1-sum(crossprob[,2]**2)
  Yes_Node_Gini <- 1-sum(crossprob[,3]**2)
  return(sum(base_prob * c(No_Node_Gini,No2_Node_Gini,Yes_Node_Gini)))
}

gini_process3_1 <-function(classes,splitvar = NULL){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- 1-sum(crossprob[,2]**2)
  Yes_Node_Gini <- 1-sum(crossprob[,1]**2)
  Yes2_Node_Gini <- 1-sum(crossprob[,3]**2)
  return(sum(base_prob * c(No_Node_Gini,Yes2_Node_Gini,Yes_Node_Gini)))
}


gini_process(data$loan)
gini_process(data$loan,data$age_40)
gini_process3(data$loan,data$edu)
gini_process3_1(data$loan,data$occup)
