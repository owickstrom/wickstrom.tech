module Sample2 where

-- start snippet animals
data Animal = Dog | Cat

isAfraidOf :: Animal -> Animal -> Bool
isAfraidOf Cat Dog = True
isAfraidOf _ _     = False
-- end snippet animals

result :: Bool
result = Dog `isAfraidOf` Cat
