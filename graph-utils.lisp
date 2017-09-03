

(defun vertex-distance (vert-a vert-b)
  (vector-distance (matrix:- (vertex-location vert-a) (vertex-location vert-b))))

