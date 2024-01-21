Generate the trajectory by utilizing the trajectory.py file, resulting in a JSON file that contains various trajectories for a specified complex state. Next, execute the get_peak_composition_and_subtrajectory.r file in R to produce the trajectory and peak composition, and save the results in a CSV file.

# Process to generate complexe state na: na?

1) generate trajectory with `Trajectory` function using state close to n, like n:n-1 or n:n-3

2) passe it to this `Extract` function to get the disire trajectory.

We use those process because when using ` T = Trajectory(na, na, 900000)` a potential issue arises when attempting to generate a complex state with the configuration "na, na." This could potentially result in a failure to generate the trajectory. To resolve this, it is recommended to utilize the 'extract' function. Please refer to the notice for further guidance.


# Discussion With Guilio for improvement, 

During our discussion, we found a trajectory 1:1>2:2>3:2>3:1>3:0, which is not included in the final CSV file with a peak composition for the complex state 3:0 . After checking the Python code to generate the trajectory, this trajectory is included in the final JSON file, but the peak composition extract function fails to retrieve the subtrajectory from this trajectory. Therefore, we could take a closer look at this code to make improvements and understand why it fails to make improvements. He also mentioned that in the final file, we could add the allele that contained a different peak composition. 
