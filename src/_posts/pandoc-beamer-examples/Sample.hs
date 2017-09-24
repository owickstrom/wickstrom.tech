module Sample where

data Animal = Dog | Cat

isAfraidOf :: Animal -> Animal -> Bool
isAfraidOf Cat Dog = True
isAfraidOf _ _     = False

result :: Bool
result = Dog `isAfraidOf` Cat
