transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' [[x], [y]] = [[x, y]]
transpose' xs = map head xs : transpose' (map tail xs)
