# Project 2

**Group Members:**

1. Adish Someshwar Rao
2. Sanjana Rao Guttalu Prasan

**What is working:**

We have implemented both the 'gossip' algorithm and the 'push-sum' algorithm for the four mentioned topologies:

1. Full Network
2. 2D Grid
3. Line
4. Imperfect 3D Grid

The program takes 3 inputs, mentioned shortly, and is run by calling the following command:

main:start(NoOfNodes, Topology, Algorithm).

1. NoOfNodes -> Any integer value (Each toplogy has its own minimum).
2. Topology -> full | line | grid_2d | grid_3d
3. Algorithm -> gossip | 'push-sum'

Results can be found in the Report.pdf file.


**What is the largest network you managed to deal with for each type of topology and algorithm?**

For both the topologies we saw quick outputs as reported below:

1. full -> 3000 actors
2. line -> 3000 actors
3. grid_2d -> 3025 actors
4. grid_3d -> 3375 actors