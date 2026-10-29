module LordOfTheRings.Location.Cards.WargLair (wargLair) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype WargLair = WargLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wargLair :: LocationCard WargLair
wargLair = location WargLair Cards.wargLair

-- | Response: After Warg Lair leaves play as an explored location, each player draws 1 card.
instance HasAbilities WargLair where
  getAbilities (WargLair a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

-- | When Revealed: Search the encounter deck and discard pile for 1 copy of Mountain Warg
-- and add it to the staging area, if able. Shuffle the encounter deck.
instance RunMessage WargLair where
  runMessage msg l@(WargLair a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead a (cardIs Enemies.mountainWarg)
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> drawCards a p 1
      pure l
    _ -> WargLair <$> runMessage msg a
