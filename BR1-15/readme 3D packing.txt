The format of these data files is(For example thpack3.txt or BR3):
 100 //Number of test problems (P)
 1 2502505 //the problem number p, seed number used in [1]
 587 233 220 //container length, width, height
 8 // number of box types n
 1 108 0 76 0 30 1 24 //box type 1, box length, 0/1 indicator,box width, 0/1 indicator,box height, 0/1 indicator,number of boxes of type i
 2 110 0 43 1 25 1 9
 3 92 1 81 1 55 1 8
 4 81 0 33 1 28 1 11
 5 120 1 99 1 73 1 11
 6 111 0 70 1 48 1 10
 7 98 0 72 1 46 1 12
 8 95 0 66 0 31 1 9//box type 8, box length, 0/1 indicator,box width, 0/1 indicator,box height, 0/1 indicator,number of boxes of type i
 2 2502605
 587 233 220
 8
 1 49 0 25 1 21 1 16
 2 60 1 51 1 41 1 8
 3 103 1 76 1 64 1 16
 4 95 1 70 1 62 1 18
 5 111 0 49 1 26 1 18
 6 85 1 84 1 72 1 16
 7 48 1 36 1 31 1 17
 8 86 0 76 0 38 1 6
.............................

After each box dimension the 0/1 indicates whether placement in the 
vertical orientation is permissible (=1) or not (=0).

[1] E.E. Bischoff and M.S.W. Ratcliff, "Issues in the development of Approaches to Container Loading", OMEGA, vol.23, no.4, (1995) pp 377-390
The data BR1~BR7 are used by our paper "ZHANG De-Fu, WEI Li-Jun, CHEN Qing-Shan, CHEN Huo-Wang. A combinational heuristic algorithm for the three-dimensional packing problem. Journal of Software.2007,18(9):2083-2089"
The data BR1~BR15 are used by our paper"ZHANG De-Fu, PENG Yu, ZHU Wen-Xing, CHEN Huo-Wang. A hybrid simulated annealing algorithm for the three-dimensional packing problem.Chinese Journal of Computers,2009, 11"