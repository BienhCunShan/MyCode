if __name__ == '__main__':
    items = [(10.0, 3.3),(1,.001),(1,.005),(1,5.0),(3.0,.01),(5.0,3.0)]
    
    print("Problem1a:")
    def testf(x):
        max_weight = 10.0 
        total_proﬁt = -1.0*sum([x[i]*items[i][0] for i in range(len(items))]) 
        total_weight = sum([x[i]*items[i][1] for i in range(len(items))]) 
        if total_weight > max_weight or x[-1] < 0.05:
            return 1000000
        else:
            return total_proﬁt/x[-1]
        
    allowed = [[0,1] for i in range(len(items))]+[[]]
    starting_temp = 10.0 
    ending_temp = 0.1 
    #np.random.seed(123)
    print("The solution, minimum and #inner iretation are:")
    print(simulated_annealing(testf,[0,0,0,0,0,0,1.5], allowed, starting_temp, ending_temp,jud=[1,1,1,1,1,1,2]))
    print("The evaluation situations are:")
    print(eval_dis(testf,allowed,jud = [1,1,1,1,1,1,2],true_val = [1, 1, 1, 0, 1, 1, 1.5]))
    
    print("Problem1b:")
    def graph_three(x):
        def jud(x,y):
            if x == y:
                return 1
            else:
                return 0
        A = [[1,1,0,0,0],
             [0,0,1,1,0],
             [0,0,1,0,1],
             [0,0,0,1,1],
             [0,0,0,0,1]]
        #B = mat(zeros((len(x)-1,len(x)-1)))
        score = 0
        for i in range(len(x)-1):
            for j in range(i,len(x)-1):
                #B[i][j-1] = jud(x[i],x[j-1])
                score = score + jud(x[i],x[j+1])*A[i][j]
        return score
    #graph_three([1,1,1,1,1,1])
    allowed = [[0,1,2] for i in range(6)]
    starting_temp = 10.0 
    ending_temp = 0.1 
    #np.random.seed(123)
    print("The solution, minimum and #inner iretation are:")
    print(simulated_annealing(graph_three,[1,1,1,1,0,1], allowed, starting_temp, ending_temp,jud=[1,1,1,1,1,1]))
    
    print("Problem2")
    print("SA")
    def testf(x):
        return(x[0]*math.sin(x[0]))
    for n in range(5):
        l = -(2*n+0.25)*math.pi
        allowed = [[l,-l]]
        starting_temp = 10.0 
        ending_temp = 0.1 
        np.random.seed(123)
        print("n=",n)
        print(simulated_annealing(testf,[0], allowed, starting_temp, ending_temp,jud=[0]))
    
    print("CD")
    def testf(x):
        return(x*math.sin(x))
    for n in range(5):
        for i in range(0,27,4):
            print("n=",n,"i=",i)
            np.random.seed(123)
            print(optimizer1D(testf,i,0.1))