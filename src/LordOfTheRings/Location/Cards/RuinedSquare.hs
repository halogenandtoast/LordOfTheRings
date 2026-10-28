module LordOfTheRings.Location.Cards.RuinedSquare (ruinedSquare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers

newtype RuinedSquare = RuinedSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinedSquare :: LocationCard RuinedSquare
ruinedSquare = location RuinedSquare Cards.ruinedSquare

-- | Surge.
-- Response: After an enemy is defeated, place 1 progress token on Ruined Square.
-- ponytail: not offered while a player controls it, where the progress would only
-- explore it again
-- Forced: If a player controls Ruined Square when a character leaves play, return Ruined
-- Square to the staging area.
instance HasAbilities RuinedSquare where
  getAbilities (RuinedSquare a) = case a.placement of
    InPlayArea _ -> [mkAbility a 2 $ forced $ CharacterLeftPlayWindow After AnyCard]
    _ -> [mkAbility a 1 $ response $ EnemyDefeatedWindow After]

instance RunMessage RuinedSquare where
  runMessage msg l@(RuinedSquare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgress a a 1
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      returnToStaging a.id
      pure l
    _ -> RuinedSquare <$> runMessage msg a
