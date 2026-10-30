module LordOfTheRings.Location.Cards.AccursedBattlefield (accursedBattlefield) where

import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Keyword (Keyword (Battle))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype AccursedBattlefield = AccursedBattlefield LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

accursedBattlefield :: LocationCard AccursedBattlefield
accursedBattlefield = location AccursedBattlefield Cards.accursedBattlefield

-- | While Accursed Battlefield is the active location, each quest card in play gains the
-- battle keyword.
instance HasModifiersFor AccursedBattlefield where
  getModifiersFor (AccursedBattlefield a) = when (a.placement == ActiveLocation) do
    quests <- getQuestCards
    for_ quests \q -> modified a q [GainKeyword Battle]

-- | Shadow: Attacking enemy gets +1 attack (+3 attack instead if the defending player's
-- threat is higher than the attacking enemy's engagement cost).
instance RunMessage AccursedBattlefield where
  runMessage msg l@(AccursedBattlefield a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      threat <- (.threat) <$> getPlayer pid
      cost <- getEngagementCost eid
      enemyGetsAttack a eid (if threat > cost then 3 else 1)
      pure l
    _ -> AccursedBattlefield <$> runMessage msg a
