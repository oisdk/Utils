import "hint" HLint.Default
import "hint" HLint.Dollar

warn = length xs > n ==> (not . null) (drop n xs) where note = IncreasesLaziness
warn = n < length xs ==> (not . null) (drop n xs) where note = IncreasesLaziness
warn = length xs < n ==> null (drop (n-1) xs) where note = IncreasesLaziness
warn = n > length xs ==> null (drop (n-1) xs) where note = IncreasesLaziness
warn = length xs >= n ==> (not . null) (drop (n-1) xs) where note = IncreasesLaziness
warn = n <= length xs ==> (not . null) (drop (n-1) xs) where note = IncreasesLaziness
warn = length xs <= n ==> null (drop n xs) where note = IncreasesLaziness
warn = n >= length xs ==> null (drop n xs) where note = IncreasesLaziness
error = a >> return b ==> a $> b
error = return a << b ==> a <$ b
error = a *> pure b ==> a $> b
error = pure a <* b ==> a <$ b
error = a *> return b ==> a $> b
error = return a <* b ==> a <$ b
