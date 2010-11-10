This file does the dependency analysis for binding groups and
type definitions. 

> module Tc.Utils.DependAnalysis (getDependentGroups) where

> import Data.Graph
> import Data.Maybe
> import Data.List(union)
> import qualified Data.Map as Map
> import Debug.Trace

Given a list of nodes, a function to convert nodes to a unique name, a function
to convert nodes to a list of names on which the node is dependendant, bindgroups
will return a list of bind groups generated from the list of nodes given.


> getDependentGroups :: (Ord name, Show name, Show node)         =>
>                       [node]           ->    -- List of nodes
>                       (node -> name)   ->    -- Function to convert nodes to a unique name
>                       (node -> [name]) ->    -- Function to return dependencies of this node
>                       [[node]]               -- dependent groups
> getDependentGroups ns getName getDeps
>       	= [ mapOnList nameToNodeFM group | group <- nameGroups ] 
>                where
>                    nameGroups = buildNameGroups nameList nameEdges 
>                    nameList = map getName ns
>                    nameEdges = buildNameEdges ns getName getDeps
>                    nameToNodeFM = Map.fromList [ (getName x, x) | x <- ns ]




Create a list of edges from a list of nodes.



> buildNameEdges :: [node]           ->    -- List of nodes
>                   (node -> name)   ->    -- Function to convert nodes to a unique name
>                   (node -> [name]) ->    -- Function to return dependencies of this node
>                   [(name,name)]          -- Edges from list of nodes.
> buildNameEdges [] _ _
> 	= []
> buildNameEdges (n:ns) getName getDeps
> 	= map mapFunc (getDeps n) ++ (buildNameEdges ns getName getDeps)
> 	where
>        mapFunc = ( \ s -> (getName n, s) )
        
        


Create a list of groups from a list of names.



> buildNameGroups :: Ord name      =>
>                    [name]        ->    -- list of names
>                    [(name,name)] ->    -- List of edges
>                    [[name]]            -- List of bindgroups
> buildNameGroups ns es
> 	= [ mapOnList intToNameFM group | group <- intGroups ] 
> 	where
>        intGroups = map preorder $ scc $ buildG (1, length ns) intEdges
>    	 intEdges = mapOnTuple nameToIntFM es
>        nameToIntFM = Map.fromList nameIntList
>    	 intToNameFM = Map.fromList [ (y,x) | (x,y) <- nameIntList ]
>      	 nameIntList = zip ns [1..]


Use a finitemap to convert a list of type A into a list of type B
NB, not being able to find an element in the FM is not considered
an error.

> mapOnList :: Ord a       =>
>              Map.Map a b ->    -- Finite map from a to b
>              [a]         ->    -- List of a
>              [b]               -- List of b
> mapOnList m as = mapMaybe (flip Map.lookup m) as



> mapOnTuple :: Ord a       =>
>               Map.Map a b ->
>               [(a,a)]     ->
>               [(b,b)]
> mapOnTuple _ [] = []
> mapOnTuple m ((a1,a2):as) = case Map.lookup a1 m of
>                                   Nothing -> mapOnTuple m as
>                                   Just x  -> case Map.lookup a2 m of
>                                                   Nothing -> mapOnTuple m as
>                                                   Just y  -> (x,y) : mapOnTuple m as
        
        
Auxiliar functions

> preorder (Node a ts) = a : preorderF ts

> preorderF           :: Forest a -> [a]
> preorderF ts         = concat (map preorder ts)

> preOrd :: Graph -> [Vertex]
> preOrd  = preorderF . dff
