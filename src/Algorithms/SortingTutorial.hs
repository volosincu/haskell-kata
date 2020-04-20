module Algorithms.SortingTutorial ( quicksort ) where


quicksort (x:xs) =
    let jumatateaMica = quicksort [i | i <- xs, i <= x]
        jumatateaMare = quicksort [i | i <- xs, i > x]
    in
        jumatateaMica ++ [x] ++ jumatateaMare