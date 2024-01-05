# Function for performing backward model selection
# ----------------------------------------------------------------------------------------------- #
# Author: Vojta Barták                                                                            #
# Institution: Faculty of Environ. Sciences, CZU Prague                                           #
# Year: 2023                                                                                      #
#                                                                                                 #
# In each step, each variable is assigned (using the drop1 function) a AIC value,                 #
# which is the AIC of the model having that variable omitted. Then if the minimum of these AIC    #
# values is not higher than the lowest AIC reached so far during the model selection plus 2,      #
# this variable is dropped from the model. (The "plus 2" ensures that a variable is dropped even  #
# when the associated AIC is actually higher than the previous AIC, but not higher than 2. This   #
# is justified because models that don't differ more that by 2 points are standardly considered   #
# as being equally supported by the data, and in such case we prefer a simpler, i.e. more parsi-  #
# monious model.)                                                                                 #
# ----------------------------------------------------------------------------------------------- #

stepAIC <- function(model, thres = 2){                                                            
  dr <- drop1(model)
  aics <- c(dr["<none>",]$AIC)
  print(paste("AIC = ", aics[length(aics)]))
  dr_var <- rownames(dr[dr$AIC==min(dr$AIC),])
  if (dr_var == "<none>" & min(dr$AIC[rownames(dr)!="<none>"]) - min(aics) < thres){
    dr_var <- rownames(dr[dr$AIC==min(dr$AIC[rownames(dr)!="<none>"]),])
  }
  if (nrow(dr) == 1) dr_var <- "<none>"
  while (dr_var != "<none>") {
    print(paste("Droping", dr_var))
    model <- update(model, paste("~.-", dr_var, sep=""))
    dr <- drop1(model)
    aics <- c(aics, dr["<none>",]$AIC)
    print(paste("AIC = ", aics[length(aics)]))
    dr_var <- rownames(dr[dr$AIC==min(dr$AIC),])
    if (dr_var == "<none>" & min(dr$AIC[rownames(dr)!="<none>"]) - min(aics) < thres){
      dr_var <- rownames(dr[dr$AIC==min(dr$AIC[rownames(dr)!="<none>"]),])
    }
    if (nrow(dr) == 1) dr_var <- "<none>"
  }
  plot(aics~c(1:length(aics)), type="l", xlab = "Step", ylab = "AIC")
  points(aics~c(1:length(aics)))
  return(model)
}

stepAIC <- function(model){
  mods <- list(model)
  dr <- drop1(model)
  aics <- c(dr[1,]$AIC)
  #aics <- AIC(model)
  print(paste("AIC = ", aics[length(aics)]))
  dr_var <- rownames(dr[dr$AIC==min(dr$AIC[rownames(dr)!="<none>"]),])
  while (length(dr_var) > 0 & dr_var != "1") {
  # while (dr_var != "<none>" & nrow(dr) > 1) {
    print(paste("Droping", dr_var))
    model <- update(model, paste("~.-", dr_var, sep=""))
    mods <- append(mods, list(model))
    (dr <- drop1(model))
    aics <- c(aics, dr[1,]$AIC)
    #aics <- c(aics, AIC(model))
    print(paste("AIC = ", aics[length(aics)]))
    dr_var <- rownames(dr[dr$AIC==min(dr$AIC[rownames(dr)!="<none>"]),])
    # print(dr_var)
  }
  plot(aics~c(1:length(aics)), type="l", xlab = "Step", ylab = "AIC")
  points(aics~c(1:length(aics)))
  points(aics[which(aics==min(aics))]~c(1:length(aics))[which(aics==min(aics))], col = "red")
  return(mods[[which(aics == min(aics))]])
}





