module LordOfTheRings.Enemy.Cards.UrukMarksman (urukMarksman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))

newtype UrukMarksman = UrukMarksman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukMarksman :: EnemyCard UrukMarksman
urukMarksman = enemy UrukMarksman Cards.urukMarksman

-- | Forced: After Uruk Marksman is turned faceup as a hidden card, deal 3 damage to a
-- character controlled by the engaged player and return Uruk Marksman to the staging area.
-- The engaged player picks the character.
instance HasAbilities UrukMarksman where
  getAbilities (UrukMarksman a) = [mkAbility a 1 $ forced $ HiddenCardTurnedFaceupWindow After (toTarget a)]

instance RunMessage UrukMarksman where
  runMessage msg e@(UrukMarksman attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> do
        dealDamageToOneCharacter attrs pid 3
        placeEnemy attrs.id StagingArea
      pure e
    _ -> UrukMarksman <$> runMessage msg attrs
