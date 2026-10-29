module LordOfTheRings.Enemy.Cards.HarborThug (harborThug) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (damageToScrollHero, scrollHolder)

newtype HarborThug = HarborThug EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

harborThug :: EnemyCard HarborThug
harborThug = enemy HarborThug Cards.harborThug

-- | Forced: When the player whose hero has Alcaron's Scroll attached raises his threat,
-- Harbor Thug engages that player.
-- Shadow: Any damage dealt by this attack is dealt to the hero with Alcaron's Scroll
-- attached, if able.
instance RunMessage HarborThug where
  runMessage msg e@(HarborThug attrs) = case msg of
    CheckWindows ws | raised@(_ : _) <- [p | Window When (ThreatRaised p) <- ws] -> do
      holder <- scrollHolder
      for_ holder \p -> when (p `elem` raised && attrs.placement /= EngagedWith p) $ engageEnemy p attrs.id
      HarborThug <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      damageToScrollHero eid defenders
      pure e
    _ -> HarborThug <$> runMessage msg attrs
