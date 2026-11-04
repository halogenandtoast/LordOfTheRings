module LordOfTheRings.Enemy.Cards.BrownWaterRats (brownWaterRats) where

import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Rat))

newtype BrownWaterRats = BrownWaterRats EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

brownWaterRats :: EnemyCard BrownWaterRats
brownWaterRats = enemy BrownWaterRats Cards.brownWaterRats

-- | Brown Water Rats cannot be damaged.
instance HasModifiersFor BrownWaterRats where
  getModifiersFor (BrownWaterRats a) = modifySelf a [CannotTakeDamage]

-- | Forced: If the players are on stage 3 and all remaining enemies have the printed Rat
-- trait, discard Brown Water Rats from play.
instance HasAbilities BrownWaterRats where
  getAbilities (BrownWaterRats a) =
    [ mkAbility a 1 $ silent $ EnemyDefeatedWindow After
    , mkAbility a 2 $ silent $ AdvancedToStageWindow After
    ]

-- | Shadow: Attacking enemy cannot be damaged this round.
instance RunMessage BrownWaterRats where
  runMessage msg e@(BrownWaterRats attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ _ -> do
      stage <- getMainStage
      enemies <- traverse getEnemy =<< select AnyEnemy
      when (stage == 3 && all ((Rat `member`) . cdCardTraits . toCardDef) enemies) $ discard attrs
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifier attrs eid CannotTakeDamage
      pure e
    _ -> BrownWaterRats <$> runMessage msg attrs
