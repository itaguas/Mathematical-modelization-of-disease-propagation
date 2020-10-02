Folder containing the long simulations (1000) done for certain conditions.

The file contour_graphs.R is used for files containing three dn values: 0.375, 0.675 and 0.75; it generates 5 files:
	state plots for each dn: plots with the number of infected, healthy and dead nodes by probability.
	node_concentration: total concentration divided by the number of infected nodes of each dn for each probability.
	total_concentration: total concentration of of each dn for each probability.
	
The file contour_graphs_nodn.R does the same as the previous, but for files with no death threshold, and generates just one state node.
