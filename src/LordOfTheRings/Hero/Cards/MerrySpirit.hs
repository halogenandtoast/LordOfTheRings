module LordOfTheRings.Hero.Cards.MerrySpirit (merrySpirit) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype MerrySpirit = MerrySpirit HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

merrySpirit :: HeroCard MerrySpirit
merrySpirit = hero MerrySpirit Cards.merrySpirit

-- | Response: After an enemy is revealed from the top of the encounter deck, exhaust Merry
-- to reduce your threat by that enemy's threat.
instance HasAbilities MerrySpirit where
  getAbilities (MerrySpirit a) = [mkAbility a 1 $ ResponseAbility (EnemyRevealedWindow After) (exhaustCost a)]

instance RunMessage MerrySpirit where
  runMessage msg h@(MerrySpirit attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (Revealed (EnemyTarget eid)) <- windows]) \eid ->
        lowerThreatBy attrs pid =<< getEnemyThreat eid
      pure h
    _ -> MerrySpirit <$> runMessage msg attrs
