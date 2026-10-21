module LordOfTheRings.Hero.Cards.Fastred (fastred) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype Fastred = Fastred HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fastred :: HeroCard Fastred
fastred = hero Fastred Cards.fastred

-- | Response: After Fastred defends an enemy attack, return that enemy to the staging
-- area to reduce your threat by 2. (Limit once per phase.)
instance HasAbilities Fastred where
  getAbilities (Fastred a) =
    [limited (GroupLimit PerPhase 1) $ mkAbility a 1 $ response $ DefendedAttackWindow After (Just $ toTarget a)]

instance RunMessage Fastred where
  runMessage msg h@(Fastred attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      enemies <- filterM (selectAny . EnemyWithId) [eid | Window _ (DefendedAttack _ eid) <- windows]
      for_ enemies \eid -> do
        placeEnemy eid StagingArea
        lowerThreat attrs.owner 2
      pure h
    _ -> Fastred <$> runMessage msg attrs
