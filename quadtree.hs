{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set

data QuadTree
  = Leaf Color Int
  | Internal Int QuadTree QuadTree QuadTree QuadTree
  deriving (Eq, Show)

data Color = Black | White
  deriving (Eq, Show)

allBlack :: Int -> QuadTree
allBlack = Leaf Black

allWhite :: Int -> QuadTree
allWhite = Leaf White

clockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
clockwise t1 t2 t3 t4 =
  let n = getEdgeLength t1
      n2 = n * 2
   in simp $ Internal n2 t1 t2 t3 t4

anticlockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
anticlockwise t1 t2 t3 t4 =
  let n = getEdgeLength t1
      n2 = n * 2
   in simp $ Internal n2 t1 t4 t3 t2

getEdgeLength :: QuadTree -> Int
getEdgeLength tree = case tree of
  Leaf _ n -> n
  Internal n _ _ _ _ -> n

simp :: QuadTree -> QuadTree
simp tree = case tree of
  Leaf _ _ -> tree
  Internal n t1 t2 t3 t4 ->
    let [t1', t2', t3', t4'] = map simp [t1, t2, t3, t4]
     in case (t1', t2', t3', t4') of
          (Leaf Black _, Leaf Black _, Leaf Black _, Leaf Black _) -> Leaf Black n
          (Leaf White _, Leaf White _, Leaf White _, Leaf White _) -> Leaf White n
          _ -> Internal n t1' t2' t3' t4'

type Position = (Int, Int)

type Id = Position

type IdColorMap = Map.Map Id Color

type PositionIdMap = Map.Map Position Id

buildMap :: QuadTree -> Position -> (IdColorMap, PositionIdMap) -> (IdColorMap, PositionIdMap)
buildMap tree position@(x, y) maps@(idColorMap, posIdMap) = case tree of
  Leaf color n ->
    let newIdColorMap = Map.insert position color idColorMap
        positions = [(x + xOffset, y + yOffset) | xOffset <- [0 .. n - 1], yOffset <- [0 .. n - 1]]
        newPosIdMap = foldl (\m k -> Map.insert k position m) posIdMap positions
     in (newIdColorMap, newPosIdMap)
  Internal n t1 t2 t3 t4 ->
    let offset = n `div` 2
        maps1 = buildMap t1 (x, y) maps
        maps2 = buildMap t2 (x + offset, y) maps1
        maps3 = buildMap t3 (x + offset, y + offset) maps2
        maps4 = buildMap t4 (x, y + offset) maps3
     in maps4

blur :: QuadTree -> QuadTree
blur tree =
  let maps = buildMap tree (0, 0) (Map.empty, Map.empty)
      newTree = tryChangeColor tree (0, 0) maps
   in newTree

tryChangeColor :: QuadTree -> Position -> (IdColorMap, PositionIdMap) -> QuadTree
tryChangeColor tree position@(x, y) maps = case tree of
  Leaf color n ->
    let colors = getNeighborColors position n maps
        newColor = determineNewColor color colors
     in Leaf newColor n
  Internal n t1 t2 t3 t4 ->
    let offset = n `div` 2
        newT1 = tryChangeColor t1 (x, y) maps
        newT2 = tryChangeColor t2 (x + offset, y) maps
        newT3 = tryChangeColor t3 (x + offset, y + offset) maps
        newT4 = tryChangeColor t4 (x, y + offset) maps
     in Internal n newT1 newT2 newT3 newT4

determineNewColor :: Color -> [Color] -> Color
determineNewColor color colors =
  let len = length colors
      numOfOppositeColors = length $ filter (/= color) colors
   in case numOfOppositeColors > len `div` 2 of
        True -> oppsiteColor color
        False -> color

oppsiteColor :: Color -> Color
oppsiteColor color =
  case color of
    Black -> White
    White -> Black

getNeighborColors :: Id -> Int -> (IdColorMap, PositionIdMap) -> [Color]
getNeighborColors id@(x, y) n maps@(idColorMap, positionIdMap) =
  let up = [(x + xoffset, y - 1) | xoffset <- [0 .. n - 1]]
      down = [(x + xoffset, y + n) | xoffset <- [0 .. n - 1]]
      left = [(x - 1, y + yoffset) | yoffset <- [0 .. n - 1]]
      right = [(x + n, y + yoffset) | yoffset <- [0 .. n - 1]]
      neighborPositions = up ++ down ++ left ++ right
      maybeIds = map (\x -> Map.lookup x positionIdMap) neighborPositions
      neighborIds = Set.toList $ Set.fromList $ catMaybes maybeIds
      colors = map (idColorMap Map.!) neighborIds
   in colors

