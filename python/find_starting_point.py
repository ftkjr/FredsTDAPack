def find_starting_point(adjacency_matrix, untraversed_points, minimum_connections, talkative = False):
    for starting_point in untraversed_points:
        if sum(adjacency_matrix[starting_point]) >= minimum_connections:
            untraversed_points.remove(starting_point)

            return starting_point, untraversed_points

    else:
        
        return None, untraversed_points
    