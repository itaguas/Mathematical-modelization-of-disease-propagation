# -*- coding: utf-8 -*-
"""
Created on Wed May  6 20:54:29 2020

@author: Nacho
"""



from itertools import chain #To unnest lists
from main import *
from scipy.integrate import solve_ivp #To integrate

n_nodes = 5 #Number of nodes
r = 0.5
k = 0.75
# all_dn = [k*0.5, k*0.9, k, k*20]
# all_dn = [k*20]
all_dn = [k*10000]
tf = 50

# p_connect = []
# pn = 100
# pa = 0.004
# pb = 0.1
# for pk in range(0, pn+1):
#     pq = np.exp((np.log(pb)-np.log(pa))/pn)
#     px = pa * pq**pk
#     p_connect.append(px)
p_connect = [0.01]

# p_rewire = np.arange(0,1,0.01)

# c = 4

# file = open("results.txt", 'w')
#newline = '\n'
#file.write(f"prob;i;dn;init_comp;final_comp;init_deg;init_cnt;final_deg;final_cnt;healthy;infected;dead;c_init;c_final{newline}")
# for prob in p_rewire:
for prob in p_connect:
    # print(prob)
    init_degree = {}
    for i in range(0,1):
        print(i)
        # global sw
        global rn
        global am
        #sw0, am0 = create_smallworld_network(n_nodes, c, prob)
        rn0, am0 = create_random_network(n_nodes, prob)
        # init_comp = [len(c) for c in sorted(nx.connected_components(sw0), key=len, reverse=True)]
        # init_deg, init_cnt = degree_dist(sw0)
        init_comp = [len(c) for c in sorted(nx.connected_components(rn0), key=len, reverse=True)]
        init_deg, init_cnt = degree_dist(rn0)
        # a = create_uniform_matrix(sw0, 0, 1)
        # b = create_uniform_matrix(sw0, 0, 1)
        a = create_uniform_matrix(rn0, 0, 1)
        # for ii in range(0, n_nodes):
        #     for j in range(0, n_nodes):
        #         if am0[ii,j] == 0:
        #             a[ii,j] = 0
        b=np.transpose(a)
        # b = create_rowcol_matrix(a, 0, 1)
        # for ii in range(0, n_nodes):
        #     for j in range(0, n_nodes):
        #         if am0[ii,j] == 0:
        #             a[ii,j] = 0
        changes = list(np.random.uniform(0, k, 1))
        # vinit = set_initial_conditions(sw0, 1, changes)
        vinit = set_initial_conditions(rn0, 1, changes)
        for dn in all_dn:
            # sw = sw0.copy()
            # print(dn)
            rn = rn0.copy()
            am = am0.copy()
            sol = solve_ivp(fun = dxdt, t_span = [0,tf], y0 = vinit, args = (am, a, b, r, k, dn), events = [equilibrium, death])
            reboot_global()
            last_sol = []
            for node in sol.y:
                last_sol.append(node[-1])
        
            while any(y >= dn for y in last_sol):
                #sw, am, new_vinit = update_network(sw, last_sol, dn)
                rn, am, new_vinit = update_network(rn, last_sol, dn)
                sol = solve_ivp(fun = dxdt, t_span = [0, tf], y0 = new_vinit, args = (am, a, b, r, k, dn), events = [equilibrium, death])
                last_sol = []
                for node in sol.y:
                    last_sol.append(node[-1])
                reboot_global()
            
            # final_comp = [len(c) for c in sorted(nx.connected_components(sw), key=len, reverse=True)]
            # final_deg, final_cnt = degree_dist(sw)
            final_comp = [len(c) for c in sorted(nx.connected_components(rn), key=len, reverse=True)]
            final_deg, final_cnt = degree_dist(rn)
            
            dead = 0
            infected = 0
            healthy = 0
            total = 0
            
            for el in last_sol:
                if el == -1:
                    dead = dead + 1
                elif el == 0:
                    healthy = healthy + 1
                else:
                    infected = infected + 1
                    total = total + el
            
            #print(dead)
            print(last_sol)
            
            #file.write(f"{prob};{i+1};{dn};{init_comp};{final_comp};{init_deg};{init_cnt};{final_deg};{final_cnt};{healthy};{infected};{dead};{changes[0]};{total}{newline}")

# file.close()

