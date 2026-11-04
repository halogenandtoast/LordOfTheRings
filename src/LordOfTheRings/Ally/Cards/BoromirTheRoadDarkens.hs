module LordOfTheRings.Ally.Cards.BoromirTheRoadDarkens (boromirTheRoadDarkens) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype BoromirTheRoadDarkens = BoromirTheRoadDarkens AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

boromirTheRoadDarkens :: AllyCard BoromirTheRoadDarkens
boromirTheRoadDarkens = ally BoromirTheRoadDarkens Cards.boromirTheRoadDarkens

-- | Boromir gets +2 [defense] while defending against an enemy with an engagement cost
-- higher than your threat.
instance HasModifiersFor BoromirTheRoadDarkens where
  getModifiersFor (BoromirTheRoadDarkens a) = for_ (placementController a.placement) \pid ->
    modifySelf a [DefenseBonusAgainstMatching (EnemyWithEngagementCostAboveThreatOf pid) 2]

-- | Response: After Boromir takes any amount of damage, ready him.
instance HasAbilities BoromirTheRoadDarkens where
  getAbilities (BoromirTheRoadDarkens a) = [mkAbility a 1 $ response $ dealtDamage a]

instance RunMessage BoromirTheRoadDarkens where
  runMessage msg s@(BoromirTheRoadDarkens a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> BoromirTheRoadDarkens <$> runMessage msg a
