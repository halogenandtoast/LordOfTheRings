module LordOfTheRings.Game where

data Game

class HasGame m where
  getGame :: m Game
