module LordOfTheRings.Location.Cards.TrollLair (trollLair) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Troll))

newtype TrollLair = TrollLair LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

trollLair :: LocationCard TrollLair
trollLair = location TrollLair Cards.trollLair

-- | While Troll Lair is in the staging area, players cannot reduce their threat.
instance HasModifiersFor TrollLair where
  getModifiersFor (TrollLair a) = when (a.placement == StagingArea) do
    players <- getRemainingPlayers
    for_ players \p -> modified a p [CannotReduceThreat]

-- | Shadow: If attacking enemy is a Troll, remove 4 damage tokens from it.
instance RunMessage TrollLair where
  runMessage msg l@(TrollLair a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      troll <- selectAny (EnemyWithId eid <> EnemyWithTrait Troll)
      when troll $ healDamage a eid 4
      pure l
    _ -> TrollLair <$> runMessage msg a
