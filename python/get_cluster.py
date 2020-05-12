def get_cluster(adjacency_matrix, minimum_connections, untraversed_points):
    ##############
    # Initialize #
    ##############
    cluster = []                               # List to store clustered points

    ########################
    # Find the first point #
    ########################
    # Start cluster with first point that meets cluster requirements
    starting_point, untraversed_points = find_starting_point(adjacency_matrix, untraversed_points, minimum_connections)
    
    # If we can't find a starting point, return None 
    if starting_point is None: 
        return None, untraversed_points
    else:
        #   otherwise append the starting point and begin search
        cluster.append(starting_point)   

    ###############################################
    # Begin the search for points in this cluster #
    ###############################################
    for point in cluster:                                                     # For points in our cluster list
        for other_point in untraversed_points:                                #   Search through the points we haven't hit
            if adjacency_matrix[point, other_point] == 1:                     #       if they are adjacent
                untraversed_points.remove(other_point)                        #           remove them from the places we haven't hit
                if sum(adjacency_matrix[other_point]) >= minimum_connections:     #           if they meet min connection requirement
                    cluster.append(other_point)                               #               add it to cluster list

    ##########
    # Return #
    ##########
    return cluster, untraversed_points
    # Fin
