# -*- coding: utf-8 -*-
"""
Created on Thu Apr 30 12:09:39 2020

@author: Nacho



This script contains the functions necesary to create a random network 
(create_random_network) or a small network (create_smallworld_network), 
in which nodes represent plants, and simulate the following model:
    dx_i/dt = r*x-i*(1-x_i/k) + sum(A_ij*x_j) - sum(B_ji*x_i)
This model is applied on the function dxdt(more details there). To solve it,
the function solve_ivp from the module scipy.integrate will be used.

The matrices A and B can be created using the functions create_uniform_matrix, 
create_rowcol_matrix (A row sums equal B col sums) and create_rowrow_matrix
(A row sums equal B row sums).

The initial conditions can be set using set_initial_conditions

To optimize the code, event handling is used. In solve_ivp, an event is a 
function that, when returns the value 0, allows us to terminate the integration.

Event functions must have the same inputs than the main function, and return a float.
I use the event function equilibrium, to terminate the integration when the solutions
stop changing (an equilibrium point is reached).

The other event function is death, which stops the integration when any node
exceeds the maximum capacity. To implement this, the function calculate_dn calculates
the maximum capacity in terms of k

The function reboot_global is used to reset the global variables (eq, vinits and t_steps);
must be done every time after solve_ivp is used, in case it will be used again

The function update_network disconnects all the dead nodes, and sets its concentration to -1

The function degree_dist calculates the degree distribution of a network
"""



import numpy as np #Support for vectors and matrices
import networkx as nx #To create network objects
import random #To generate random numbers
import collections
# import matplotlib.pyplot as plt


#First, define two global variables that will be necessary to stop the integration when an equilibrium is reached
#These are defined as global variables because dxdt cannot return more than one object (otherwise, solve_ivp will not work)
global vinits
vinits = [] #Array that will contain integration results
global eq
eq = 1 #Will become 0 when an equilibrium is reached

global t_steps #Global variable to keep track of the process when the program is running
t_steps = [] #List that will contain the already calculated time steps, as integers



def dxdt(t, vinit, am, ma, mb, r, k, dn):

    #Function that represents the right side of the following equation:
    #dx_i/dt = r*x_i*(1-x_i/k) + sum(A_ij*x_j) - sum(B_ji*x_i)
    #where:
        #x_i is a given node (we are working on a network)
        #x_j are the nodes connected to x_i
        #r and k are scalars
        #A and B are matrixes of the same shape as the adjacency matrix of the network
    #This model gives the concentration of a pathogen in all the nodes of a network at any given time
    
    
    #The function also keeps all the vinit that it receives (that is, the consecutive integration results) in the global array vinits
    #When the first solutions of 5 consecutive whole time steps are the same (rounded to 3 decimals), the global variable eq is set to 0
    
    #Input:
        ###vinit: matrix of one column and one row per node of the network. Contains the initial conditions (initial pathogen concentration on each node)
        ###t: variable that is not used in the function, but is needed to later use the method odeint
        ###am: adjacency matrix
        ###ma, mb: matrices of the same shape as am
        ###r, k: scalars
        ###n_eq: integer; number of previous solutions that must be equal to the actual solution to stop the integration
        ###n_dec: number of decimals to which round the solutions to make the comparisons
        ###dn: input necessary for the function death
    
    #Output:
        ###vinit: the updated vinit matrix, containing the final pathogen concentration on each node assuming that the result of this right side of the equation equals x_I
    
    global vinits
    global t_steps
    
    #The integration function receives an initial and a final time, but does not do one integration per time; for example if the time interval is [0,2] it may do many integration step, at times like 0.234, 0.3454...
    t_step = int(t) #t_step takes only the integer part of the current time
    # print("ALLL TIMES:", t)
    
    if t_step not in t_steps:
        # print("time:", t)
        #print(t_step) #Prints an integer when the first time step greater than that value is reached
        t_steps.append(t_step) #Appends that integer to t_steps, so that it will not be repeated
        vinits.append(vinit) #Appends the integration solution to vinits; therefore, the solutions of vinits are not to close to each other
    
    global eq #Import eq
    
    final = np.zeros(np.size(vinit)) #Create the vector that will contain the output
    
    
    for i in range(0, np.size(vinit, 0)): #Iterate over the elements of vinit (the nodes)
        
        if am.sum(1)[i] != 0: #Check if the node is dead (value of -1)
            h = 0 #sum(A_ij*x_j)
            g = 0 #sum(B_ji*x_i)
        
            for j in range(0, np.size(am, 1)): #Iterate over the columns of am, ma and mb
                if am[i,j] != 0: #If the two nodes are connected
                    h = h + ma[i,j]*vinit[j]
                    g = g + mb[j,i]*vinit[i]
                
            final[i] = r*vinit[i]*(1-vinit[i]/k) + h - g #Update final
        
        else: #If the node is dead, its concentration stays the same
            if vinit[i] == 0:
                final[i] = 0
            else:
                print(vinit[i])
                final[i] = r*vinit[i]*(1-vinit[i]/k)
            final[i] = 0
    
    #Check if eq must be set to 0
    try:
        vinits[-10] #To avoid stopping in the first steps (otherwise, it always stop after two steps)
        if np.all(abs(vinits[-10:] - vinit) < 10e-4): #When the last 4 integration results of vinits (which are not entirely consecutive) are similar enough to the new result, equilibrium is assumed  
            eq = 0 #Set eq to 0, so that equilibrium will return 0, and the integration will stop
            
    except:
        pass
        
    return final



def create_random_network(n_nodes = 100, p_connect = 0.05):
    
    #Function that creates a random network and its adjacency matrix
    
    #Input:
        ###n_nodes: an integer, the number of nodes
        ###p_connect: a float, the probability to draw an edge between two nodes
    
    #Output:
        ###rn: the network
        ###am: the adjacency matrix
    
    rn = nx.gnp_random_graph (n_nodes, p_connect) #Create random network
    am = nx.to_numpy_matrix(rn) #Get the adyacency matrix
    
    return rn, am



def create_smallworld_network(n_nodes = 100, c = 4, p_rewire = 0.05):
    
    #Function that creates a small world network and its adjacency matrix
    
    #Input:
        ###n_nodes: an integer, the number of nodes
        ###c: de initial degree of each node
        ###p_rewire: a float, the probability of rewiring each edge
    
    #Output:
        ###sw: the network
        ###am: the adjacency matrix
    
    sw = nx.watts_strogatz_graph(n_nodes, c, p_rewire) #Create random network
    am = nx.to_numpy_matrix(sw) #Get the adyacency matrix
    
    return sw, am



def create_uniform_matrix(network, dmin = 0, dmax = 1):
    
    #Function that creates a matrix of the same shape than the adjacency matrix of a network
    #Its values follow a uniform distribution
    #The diagonal elements are 0
    
    #Input:
        ###dmin: a scalar, the lower limit of the uniform distribution
        ###dmax: a scalar, upper limit of the uniform distribution
        ###rn: network
        
    #Output:
        ###unifm: the matrix
        

    
    n_nodes = nx.number_of_nodes(network) #Get the number of nodes
    unifm = np.random.uniform(dmin,dmax, size = (n_nodes, n_nodes)) #Create vector with uniform distribution
    np.fill_diagonal(unifm, 0) #Set the diagonal to 0
    
    return unifm



def create_rowcol_matrix(matrix, dmin = 0, dmax = 1):
    
    #Function that takes a matrix, and creates another one of the same shape and a diagonal of 0s; the sum of each row of the first matrix equals the sum of each column of the second matrix
    #Its values follow a uniform distribution
    
    #Input:
        ###matrix: a matrix
        ###dmin: a scalar, the lower limit of the uniform distribution
        ###dmax: a scalar, upper limit of the uniform distribution
    
    #Output:
        ###unifm: the matrix
    
    rowsum = matrix.sum(1) #Addition of each row of the original matrix
    
    n_nodes = np.shape(matrix)[0] #Get the number of nodes
    unifm = np.random.uniform(dmin, dmax, size = (n_nodes, n_nodes)) #Create vector with uniform distribution
    np.fill_diagonal(unifm, 0) #Fill the diagonal with 0s
    for i in range(0, n_nodes):
        for j in range(0, n_nodes):
            if matrix[i, j] == 0:
                unifm[j, i] = 0
    aa = 0
    for el in unifm.sum(0):
        if el != 0:
            unifm[:,aa] = unifm[:,aa]/el #Normalize the columns of unifm
        aa = aa + 1
    unifm = unifm*rowsum #Multiply by the addition of each row of the first matrix
    return unifm



def create_rowrow_matrix(matrix, dmin = 0, dmax = 1):
    
    #Function that takes a matrix, and creates another one of the same shape and a diagonal of 0s; the sum of each row of the first matrix equals the sum of each row of the second matrix
    #Its values follow a uniform distribution
    
    #Input:
        ###matrix: a matrix
        ###dmin: a scalar, the lower limit of the uniform distribution
        ###dmax: a scalar, upper limit of the uniform distribution
    
    #Output:
        ###unifm: the matrix
    
    n_nodes = np.shape(matrix)[0] #Get the number of nodes
    
    rowsum = np.reshape(matrix.sum(1), (n_nodes, 1)) #Addition of each row of the original matrix, and reshaping to just one column
    
    unifm = np.random.uniform(dmin, dmax, size = (n_nodes, n_nodes)) #Create vector with uniform distribution
    np.fill_diagonal(unifm, 0) #Fill the diagonal with 0s
    for i in range(0, n_nodes):
        for j in range(0, n_nodes):
            if matrix[i, j] == 0:
                unifm[j, i] = 0
                
    aa = 0
    for el in unifm.sum(1):
        if el != 0:
            unifm[aa,] = unifm[aa,]/el
        aa = aa + 1
    unifm = unifm*rowsum #Multiply by the addition of each row of the first matrix
    
    return unifm




def set_initial_conditions(network, n_changes = 1, ci = 1):
    
    #Function that creates the initial conditions for the model
    
    #Input:
        ###network: a network
        ###n_changes: a scalar, number of nodes that must be updated
        ###ci: can be:
            #####A scalar: all the changes will be done to that scalar
            #####A list: the length of ci must iqual n_changes. Each node will have a value of ci
    
    #Output:
        ###vinit: a matrix of 1 column and as many rows as nodes in the network. A number of nodes equal to n_changes will have one of the given values, and the rest will be 0
    
    n_nodes = nx.number_of_nodes(network) #Get the number of nodes
    
    #Check if the number of changes is not greater than the number of nodes
    if n_changes > n_nodes:
        print("The number of changes cannot be greater than the number of nodes")
        return
    
    vinit = np.zeros(n_nodes) #Create vinit, filled with 0s
    
    
    for i in range(0,n_changes): #Change as many nodes as n_changes indicates
        if type(ci) == int or type(ci) == float: #If ci is a number
            initial = random.randint(0,n_nodes - 1)
            while vinit[initial] != 0: #Check if the node has already been updated
                initial = random.randint(0,n_nodes-1)
            vinit[initial] = ci 
        elif type(ci) == list: #If ci is a list
            if n_changes == len(ci):
                for num in ci:
                    initial = random.randint(0,n_nodes - 1)
                    while vinit[initial] != 0:
                        initial = random.randint(0,n_nodes -1)
                    vinit[initial] = num
                break #exit the for loop that iterates between 0 and n_changes (i)
            else: #If ci is neither a number nor a list
                print("The number of changes must be the same as the length of ci")
                return
    return vinit



def reboot_global():
    
    #Function that resets the global variables: eq becomes 1, and vinits and t_steps empty lists
    #This must be done after each solve_ivp function, in case it will be ran again
    
    global eq
    eq = 1
    
    global vinits
    vinits = []
    
    global t_steps
    t_steps = []
    
    return



def calculate_dn (k, mul):
    
    #Function that calculates dn, which represents the concentration of pathogen that kills a node
    
    #Input:
        #k: a scalar, the same parameter used in dxdt
        #mul: the number by which k must be multiplied to obtain dn. Should be 1 or less
    
    dn = k*mul
    
    return dn
    


def equilibrium (t, vinit, am, ma, mb, r, k, dn):
    
    #Function that will be used as event in solve_ivp.
    #Returns the global variable eq, that will be set to 0 by dxdt when an equilibrium is reached
    
    #Input: the same as dxdt, although none are needed (if the inputs are not the same solve_ivp will raise an error)
    
    #Output: eq, that will be either 1 or 0; when it is 0, the integration will stop
    
    global eq #Import eq
    
    return eq

equilibrium.terminal = True #When equilibrium returns 0, the integration stops



def death (t, vinit, am, ma, mb, r, k, dn):
    
    #Function that will be used as event in solve_ivp.
    #When any node exceeds a certain limit, the integration stops
    
    #Input: the same as dxdt, although none are needed (if the inputs are not the same solve_ivp will raise an error)
        ###dn: when any node equals or suprasses this value, the integration stops
    
    #Output: 0 if any node is equal or greater than dn, 1 otherwise

    for el in vinit:
        if el >= dn:
            return 0
    return 1

death.terminal = True #When equilibrium returns 0, the integration stops



def update_network (rn, vinit, dn):
    
    #Function that removes all the edges of the dead nodes, and sets its concentration to -1
    
    #Input:
        ###rn: the network
        ###vinit: the actual conditions of each node
        ###dn: the value above which a node is considered dead
    
    #Output:
        ###rn: the updated network
        ###am: the updated adjacency matrix
        ###vinit: the updated concentrations, where the dead nodes have -1
    
    for i in range(0,len(vinit)): #Pass through all the nodes
        if vinit[i] >= dn: #Check if any node is dead
            rn.remove_edges_from(list(rn.edges(i))) #Remove its edges
            vinit[i] = -1 #Set its value to -1
    
    am = nx.to_numpy_matrix(rn) #Create the updated adjacency matrix
    
    return rn, am, vinit



def degree_dist (rn):
    
    #Function that calculates the degree distribution of a network
    
    #Input:
        ###rn: the network
    
    #Output:
        ###deg: tuple containing the different degree values
        ###cnt: tuple containing the number of nodes that have each degree value; it has the same length as deg, and the order is the same
    
    degree_sequence = sorted([d for n, d in rn.degree()], reverse = True)
    degreeCount = collections.Counter(degree_sequence)
    deg, cnt = zip(*degreeCount.items())
    
    return deg, cnt