module LordOfTheRings.Trait (module LordOfTheRings.Trait) where

import LordOfTheRings.Prelude

data Trait
  = Archer
  | Armor
  | Artifact
  | Beorning
  | Burden
  | Condition
  | Craftsman
  | Creature
  | DolGuldur
  | Dunedain
  | Dungeon
  | Dwarf
  | Forest
  | Goblin
  | Gondor
  | Hobbit
  | Insect
  | Istari
  | Item
  | Marshland
  | Minstrel
  | Mountain
  | Nazgul
  | Noble
  | Noldor
  | Orc
  | Ranger
  | Riverland
  | Rohan
  | Scout
  | Signal
  | Silvan
  | Skill
  | Spider
  | Steward
  | Stronghold
  | Title
  | Trap
  | Troll
  | Undead
  | Warrior
  | Wasteland
  | Weapon
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
