import random
from itertools import combinations_with_replacement
import numpy as np

def Trajectory(nA:int,nB:int, rep:int)->list[list]: 
    """
    Generate all the evolutionary trajectories for a certain complex state  starting from wildtype 1:1.

    Args:
    -nA and nB are integer values representing the major and the minor allele of the final state of the complex state CNA. 
    clonal CNA segments has the the form nA:nB=>(nA,nB)
    - rep: rep is an integer value representing the number of repetitions to get all the trajectory.
    Returns:
    - all_trajectory: a list of all possible trajectories for the evolutionary, where each trajectory is
                      represented as a list of tuples (nA,nB)
    """

    all_trajectory =[]  # initialize an empty list to store all trajectories

    # determine the maximun and minum number of copy numbers between nA and nB
    final1, final2 = max(nA,nB), min(nA,nB)
    
    for _ in range(rep):

        trajectory = [(1,1)] # initialize a trajectory with heterozygous normal states 1:1
        l=[]
        step = 0 # index of the last tuple in the trajectory
        stop = False
        Temp=[1,1] # Temps will save the curent CNA that will be used as reference for the next state.
# By repeating the trajectory generation process "rep" times, it is expected that all possible trajectory 
# combinations will be obtained.

        while stop==False:
            # get the minimum and maximum  number of copy between nA and nB from the current state
            Min, Max = min(trajectory[step]), max(trajectory[step])

            # We need to exit the loop if the trajectory is incorrect, which occurs when either the minimum number of copies is 
            # greater than max(nA,nB)+1 or the minimum is greater than min(nA,nB)+1.
            if (Max>final1+1 or Min > final1+1):
                stop = True
                break
            # if we reach the final complex state,stopping condition is met, add final state to the trajectory and exit loop
            elif((Max==final1 ) and (Min-1 == final2 or Min == final2 
                                                     or Min+1 == final2) ):
                if nA>nB and (nB,nA) not in trajectory:
                    trajectory.append((nB,nA))
                elif nA<nB and (nA,nB) not in trajectory:
                    trajectory.append((nA,nB))
                break

            elif ((Min-1)>=0):
                # generate all possible combinations of (nA,nB) for the next step
                temp = combinations_with_replacement(np.arange((Min-1), (Max+2)), 2)
            elif(Min==0):
                # generate all possible combinations of (nA,nB) for the next step
                temp = combinations_with_replacement(np.arange(Max+2), 2)

            l = [i for i in temp if min(i) > 0] # select all (nA,nB) exluded configurations with loss of heterozygosity
            # randomly select a possible state (nA,nB)
            element = random.choice(l)
            # if the selected combination (nA,nB) is not already in the trajectory,
            # and satisfy certain conditions, add (nA,nB) to the trajectory
            # and update the step and Temp variables

            if element not in trajectory:
                if(element[0]>=(Temp[0]-1) and element[1]>=(Temp[1]-1) and element[0]<=2*Temp[0] and 
                   element[1]<=2*Temp[1]):
                    trajectory.append(element)
                    step += 1
                    Temp=element

        # if stopping condition is not met, add trajectory to the list
        # if we have a new valide trajectory we add in all trajectory          
        if stop == False and trajectory not in all_trajectory and nA<nB:
            # trajectory[-1]=(nA,nB)
            all_trajectory.append(trajectory)

        # if stopping condition is not met, and nB>nA, add inverted trajectory to the list
        elif(stop == False and nA>nB):
            res = [(sub[1], sub[0]) for sub in trajectory]
            if res not in all_trajectory:
                all_trajectory.append(res)

    return all_trajectory

if __name__ == "__main__":
    T=Trajectory(3,1, 2000)
    n= len(T)
    print(f'The number of trajectory is : {n}')
    for i in range(n):
        print(T[i])
    
    