module LordOfTheRings.Location.Cards.OutskirtsOfArchet (outskirtsOfArchet) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype OutskirtsOfArchet = OutskirtsOfArchet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

outskirtsOfArchet :: LocationCard OutskirtsOfArchet
outskirtsOfArchet = location OutskirtsOfArchet Cards.outskirtsOfArchet

-- | Travel: Search the encounter deck, discard pile and victory display for Protect the
-- Bree-landers, and reveal it.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, raise your
-- threat by 3.
instance RunMessage OutskirtsOfArchet where
  runMessage msg l@(OutskirtsOfArchet a) = case msg of
    TravelTo lid | lid == a.id -> do
      searchAndRevealEncounterCard (cardIs Quests.protectTheBreeLanders)
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == a.cardId -> do
      encounterRaisesThreat pid 3
      pure l
    _ -> OutskirtsOfArchet <$> runMessage msg a
