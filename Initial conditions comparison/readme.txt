Probabilities: from 0'004 to 0'1, 100 probabilities with a logarithmic growth
Number of simulations: 1000 for each probability

Number of nodes: 100
r = 0'5
k = 0'75
dn = 0'75
A: uniform distribution between 0 and 1
B: uniform distribution between 0 and 1
Initial conditions: for one file one infected node with an initial viral load between 0 and 0'75, for the other between 0 and 0'375

This was done to see the effect of the initial viral load.

The file vinit_comparison.R generates 3 files, all of them comparing both situations:
	state_comparison.jpg: compares the number of healthy, infected and dead nodes for each probability. There is no difference.
	node_concentration_comparison: compares the total concentration divided by the number of infected nodes for each probability. When the initial conditions are between 0 and k, the concentration is always higher.
	total_concentration_comparison: compares the total concentration for each probability. Both lines start the same, but as the probability grows higher, the line corresponding to initial load between 0 and k is higher.
