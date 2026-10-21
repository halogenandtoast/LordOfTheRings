module LordOfTheRings.Hero.Cards.Mablung (mablung) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Mablung = Mablung HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mablung :: HeroCard Mablung
mablung = hero Mablung Cards.mablung

-- | Response: After you engage an enemy, add 1 resource to Mablung's resource pool.
-- (Limit once per phase.)
instance HasAbilities Mablung where
  getAbilities (Mablung a) = [limited (PlayerLimit PerPhase 1) $ mkAbility a 1 $ response (PlayerEngagedEnemyWindow After a.owner)]

instance RunMessage Mablung where
  runMessage msg h@(Mablung attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure h
    _ -> Mablung <$> runMessage msg attrs
