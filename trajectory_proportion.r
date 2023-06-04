source('function.r') # inmport all function from function.r
library("rjson")

##### generate trajectory 
CCF_1=1
purity=1

result <- fromJSON(file = "json_trajectory53.json") # We are reading a file that contains all the trajectories generated 
#using trajectory.py for a specific complex state.
Len <-length(result)-1
for (i in 5:6) { # this is used to print all the different possible 
  #trajectory for the a given complex state
  print(result[[paste0("Id",as.character(i))]])
  print(i)
  
}

finalstate<-c() #This will include the end state that has undergone various 
#mutations for a particular sub-path. This is because a final state of 3:1
#can correspond to different combinations such as A1A2A3:B1 or A1:B1B2B3,
#or even more complex scenarios like A2:B1B2B4.

for (pp in 1:50){
  print(paste0('pp is :',pp))
  traj<-result[[paste0("Id",as.character(pp))]] # select a trajectory
  
  
  initialSate= initial_state("1:1")
  traject<-c() # this will record different sub-trajectory
  compt<-0
  while(compt<=20){ # 20 is the number of repetition to ensure to record all
    #different possible sub-trajectory
    trajectory<-c(join_strings(get_alleles(initialSate))) # initiate a sub-trajectory with A1:B1
    
    for(i in 2:length(traj)){ # we start from 2 since the first state of traj is 1:1
      if(i==2){ # If we are at the beginning of the loop, execute this scope.  
        t_2= evolve(initialSate, traj[i])
      }else{ # run this scope for other 
        t_2= evolve(t_2[[position]], traj[i])
      }
      #As the evolve function may provide multiple combinations of copy states, such as 
      #A1A2A3:B1 or A1:B1B2B3, or A2:B1B2B4, depending on the step, we randomly choose one of them 
      #for each repetition to create a linear trajectory. By repeating this process, we ensure that 
      #all position trajectories are recorded. The "position" refers to the index of a copy state that is used for the next step.
      position<-sample(1:length(t_2), 1)
      
      # The following line of code help to avoid the following scenario
      #\"A1B1\", \"A1B1B2\", \"A1A2A3B1B2\", \"A1A2A3B1\" this one of example
      #\"A1B1\", \"A1A2B1\", \"A1A2B1B2B3\", \"A2B1B2B3B4\"
      
      temp1<-gsub("[[:digit:]]", "", trajectory[i-1])
      oldncopyA<- lengths(regmatches(temp1, gregexpr("A", temp1)))
      oldncopyB<-lengths(regmatches(temp1, gregexpr("B", temp1)))
      
      temp2 <- gsub("[[:digit:]]", "", join_strings(get_alleles(t_2[[position]])))
      newncopyA<- lengths(regmatches(temp2, gregexpr("A", temp2)))
      newncopyB<-lengths(regmatches(temp2, gregexpr("B", temp2)))
      
      while(2*oldncopyA<newncopyA || 2*oldncopyB<newncopyB){
        position<-sample(1:length(t_2), 1)
        temp2 <- gsub("[[:digit:]]", "", join_strings(get_alleles(t_2[[position]])))
        newncopyA<- lengths(regmatches(temp2, gregexpr("A", temp2)))
        newncopyB<-lengths(regmatches(temp2, gregexpr("B", temp2)))
      }
      
      # add the alleles of the selected copy states in trajectory
      trajectory<-c(trajectory,join_strings(get_alleles(t_2[[position]]))) 
    }
    
    if(compt==0){ # if it is the first iteration of the loop, add the sub-trajectory in traject
      traject<-c(traject,list(trajectory))
      finalstate<-c(finalstate,t_2[position]) #We add the end copy state of the sub-trajectory to the list.
    } else{ #In case we are not in the first iteration, we verify if the sub-trajectory exists in the "trajectory" list.
      #This is done to ensure that we have unique trajectories.
      is_in_list_of_lists <- FALSE
      for (l in traject) {
        if (identical(l, trajectory)) {
          is_in_list_of_lists <- TRUE
          break
        }
      }
      if (!is_in_list_of_lists) { # if the sub-trajectory is not in traject, we append the new sub-trajectory
        traject<-c(traject,list(trajectory))
        finalstate<-c(finalstate,t_2[position]) # We add the end copy state of the sub-trajectory to the list.
      }
    }
    
    compt<-compt+1
  }
}
has4peak<-0
has3peak<-0
has5peak<-0
numbersubtrajectory<-0
for(i in 1:length(finalstate)){
  t<-get_peaks(finalstate[i][[1]], CCF_1, purity)$peak
  
  if (length(t)==4){
    has4peak<-has4peak+1
  }else if(length(t)==3){
    has3peak<-has3peak + 1
    print(t)
  }
  else if(length(t)==5){
    has5peak<-has5peak + 1
  }
  numbersubtrajectory <- numbersubtrajectory +1
  
}
print(paste0('proportion for (0.2,0.6) is:',has2peak/numbersubtrajectory))
print(paste0('proportion for (0.2,0.4,0.6) is:',has3peak/numbersubtrajectory))
print(paste0('Number of subtrajectory is ',numbersubtrajectory))
t<-get_peaks(finalstate[i][[1]], CCF_1, purity)

# Read genotypes
genotypes <- t%>% select(genotype_1)
genotypes
ex_baf<-expected_baf(purity,CCF_1,peaks_df = t)
expected_dr(t$karyotype_1,purity,CCF_1)
