def FredsDBSCAN(adjacency_matrix, minimum_connections):

    ##############
    # Initialize #
    ##############
    cluster_list = []                                                        # List of Clusters
    cluster = []                                                             # Holds each Cluster as we iterate
    untraversed_points = [p for p in range(len(adjacency_matrix))]           # List of points to hit
    
    #################################
    # Traverse our Adjacency Matrix #
    #################################

    while len(untraversed_points) >= minimum_connections:                    # While there's a possibility of a cluster 
        cluster, untraversed_points = get_cluster(adjacency_matrix, 
                                                minimum_connections, 
                                                untraversed_points)          #     find clusters and give back list of untraversed points
        if cluster is None:                                                  #     if we didn't find any clusters 
            break                                                            #         STAHP
        else:                                                                #     assuming we found a cluster
            cluster_list.append(cluster)                                     #         add it to our list of clusters

    return cluster_list