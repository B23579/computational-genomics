import random
from itertools import combinations_with_replacement
import numpy as np

def Trajectory(Final1:int,Final2:int, rep:int)->list[list]: 
    """
    Generate all the evolutionary trajectories for a certain complex state  starting from wildtype 1:1.

    Args:
    - Final1: integer value of the final state
    - Final2: integer value of the final state
    - rep: integer value representing the number of repetitions for the trajectory

    Returns:
    - all_trajectory: a list of all possible trajectories for the evolutionary, where each trajectory is
                      represented as a list of tuples (index 1,index 2)
    """

    all_trajectory =[]  # initialize an empty list to store all trajectories
    final1, final2 = max(Final1,Final2), min(Final1,Final2)
    for i in range(rep):

        trajectory = [(1,1)] # initialize a trajectory with starting positions
        l=[]
        step = 0
        stop = False
        Temp=[1,1]

        while stop==False:
            Min, Max = min(trajectory[step]), max(trajectory[step])
            if (Max>final1+1 or Min > final1+1):
                stop = True
                break

            elif((Max==final1 ) and (Min-1 == final2 or Min == final2 
                                                     or Min+1 == final2) ):
                if (final2,final1) not in trajectory:
                    trajectory.append((final2,final1))
                break
            elif ((Min-1)>=0):
                temp = combinations_with_replacement(np.arange((Min-1), (Max+2)), 2)
            elif(Min==0):
                temp = combinations_with_replacement(np.arange(Max+2), 2)
            l = [i for i in temp if min(i) > 0]
            element = random.choice(l)

            if element not in trajectory:
                if(element[0]>=(Temp[0]-1) and element[1]>=(Temp[1]-1) and element[0]<=2*Temp[0] and 
                   element[1]<=2*Temp[1]):
                    trajectory.append(element)
                    step += 1
                    Temp=element

        # if stopping condition is not met, add trajectory to the list          
        if stop == False and trajectory not in all_trajectory and Final1>Final2:
            all_trajectory.append(trajectory)

        # if stopping condition is not met, and Final2>Final1, add inverted trajectory to the list
        elif(stop == False and Final1<Final2):
            res = [(sub[1], sub[0]) for sub in trajectory]
            if res not in all_trajectory:
                all_trajectory.append(res)

    return all_trajectory

if __name__ == "__main__":
    T=Trajectory(3,1, 2000)
    n= len(T)
    for i in range(n):
        print(T[i])
    
    