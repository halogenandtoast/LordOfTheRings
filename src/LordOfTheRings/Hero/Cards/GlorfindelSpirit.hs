module LordOfTheRings.Hero.Cards.GlorfindelSpirit (glorfindelSpirit) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype GlorfindelSpirit = GlorfindelSpirit HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

glorfindelSpirit :: HeroCard GlorfindelSpirit
glorfindelSpirit = hero GlorfindelSpirit Cards.glorfindelSpirit

-- | Forced: After Glorfindel exhausts to commit to a quest, raise your threat by 1. One
-- committed without exhausting, as Light of Valinor lets him, is still ready.
instance HasAbilities GlorfindelSpirit where
  getAbilities (GlorfindelSpirit a) =
    [restricted a 1 (exists $ HeroWithId a.id <> ExhaustedHero) $ forced $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage GlorfindelSpirit where
  runMessage msg h@(GlorfindelSpirit attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      raiseThreat pid 1
      pure h
    _ -> GlorfindelSpirit <$> runMessage msg attrs
