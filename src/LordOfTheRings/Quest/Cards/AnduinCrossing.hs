module LordOfTheRings.Quest.Cards.AnduinCrossing (anduinCrossing) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (EastBank, WestBank))

newtype AnduinCrossing = AnduinCrossing QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

anduinCrossing :: QuestCard AnduinCrossing
anduinCrossing = quest AnduinCrossing Cards.anduinCrossing

instance HasModifiersFor AnduinCrossing where
  getModifiersFor (AnduinCrossing a) = when a.flipped do
    banks <- select (LocationMatchAny [LocationWithTrait EastBank, LocationWithTrait WestBank])
    for_ banks \lid -> modified a lid [CannotBeTraveledTo]
    modifySelf a [NoProgressFromCardEffects]
    active <- select IsActiveLocation
    for_ active \lid -> modified a lid [NoProgressFromCardEffects]
    players <- getPlayerOrder
    for_ players \pid -> modified a (PlayerTarget pid) [CommitRequiresDiscard a.id (cardIs Objectives.rangerOfIthilien)]
