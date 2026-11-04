module LordOfTheRings.Location.Cards.MumakTrail (mumakTrail) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype MumakTrail = MumakTrail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mumakTrail :: LocationCard MumakTrail
mumakTrail = location MumakTrail Cards.mumakTrail

-- | While Mûmak Trail is the active location, it gains: "Response: When Mûmak Trail leaves
-- play as an explored location, search the encounter deck and discard pile for a Wild
-- Mûmak and add it to the staging area. Shuffle the encounter deck."
instance HasAbilities MumakTrail where
  getAbilities (MumakTrail a) = [mkAbility a 1 $ response $ ExploredWindow When a.id | a.placement == ActiveLocation]

instance RunMessage MumakTrail where
  runMessage msg l@(MumakTrail a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchEncounterDeckAndDiscard pid a (cardIs Enemies.wildMumak)
      pure l
    _ -> MumakTrail <$> runMessage msg a
