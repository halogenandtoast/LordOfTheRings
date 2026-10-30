module LordOfTheRings.Location.Cards.NorthDowns (northDowns) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight)

newtype NorthDowns = NorthDowns LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

northDowns :: LocationCard NorthDowns
northDowns = location NorthDowns Cards.northDowns

-- | While it is Night, North Downs gets +2 threat.
instance HasModifiersFor NorthDowns where
  getModifiersFor (NorthDowns a) = whenM isNight $ modified a a [ThreatModifier 2]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, remove all
-- progress from the current quest.
instance RunMessage NorthDowns where
  runMessage msg l@(NorthDowns a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      -- as much as there is: it removes no more than the quest has
      removeProgressFromQuest (CardIdSource cid) maxBound
      pure l
    _ -> NorthDowns <$> runMessage msg a
