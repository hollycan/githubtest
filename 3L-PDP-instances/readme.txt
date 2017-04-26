54 3L-PDP instances by D. M�nnel and A. Bortfeldt *)

    // 1. Line: General data
    //    Maximum number of routes
    //    Number of requests **)
    //    Maximum load weight of vehicle
    //    Length of loading space
    //    Width of loading space
    //    Height of loading space

    // 2. Line: Data of depot
    //    Number of depot (= 0)
    //    X-coordinate of depot
    //    Y-coordinate of depot
    //    Number of depot (= 0)
    //    X-coordinate of depot
    //    Y-coordinate of depot
    //    Maximum duration of route
    //    Obsolete parameter 

    // 3.-n. Line: Data of requests
    //    Number of Request
    //    X-coordinate of pickup node
    //    Y-coordinate of pickup node
    //    Service time at pickup node
    //    X-coordinate of delivery node
    //    Y-coordinate of delivery node
    //    Service time at delivery node
    //    Sum of weights of the boxes 
    //    Number of boxes 
    //    Length of box 1
    //    Width of box 1
    //    Height of box 1
    //    Fragility of box 1 (0/1=false/true)
    //    Length of box 2
    //    Width of box 2
    //    Height of box 2
    //    Fragility of box 2 (0/1=false/true)
    //    etc. ...(more 4-digit blocks if more boxes exist)

*) See: M�nnel, D.; Bortfeldt, A. (2015):
   A hybrid algorithm for the vehicle routing problem with 
   pickup and delivery and 3D loading constraints.
   Working Paper No. 15/2015, Fakult�t f�r Wirtschaftswissenschaft, 
   Otto-von-Guericke Universit�t Magdeburg. 
    
**) Please note that some of the instances with 50/75/100 requests 
   have a slightly higher number of requests than 50/75/100 
   respectively because we derived some instances from the Li-Lim-
   VRPPDTW instances and adopted the original request numbers.