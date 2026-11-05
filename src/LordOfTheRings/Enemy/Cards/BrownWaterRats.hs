module LordOfTheRings.Enemy.Cards.BrownWaterRats (brownWaterRats) where

import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
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
-- trait, discard Brown Water Rats from play. It can come true only as the players reach a
-- stage or as another enemy leaves play, which is read as it goes, without it.
instance HasAbilities BrownWaterRats where
  getAbilities (BrownWaterRats a) = [mkAbility a 1 $ silent $ AdvancedToStageWindow After]

-- | Whether only Rats are left, once the enemies leaving have gone.
check :: ReverseQueue m => EnemyAttrs -> [EnemyId] -> m ()
check attrs leaving = do
  stage <- getMainStage
  enemies <- traverse getEnemy . filter (`notElem` leaving) =<< select AnyEnemy
  when (stage == 3 && all ((Rat `member`) . cdCardTraits . toCardDef) enemies) $ discard attrs attrs

-- | Shadow: Attacking enemy cannot be damaged this round.
instance RunMessage BrownWaterRats where
  runMessage msg e@(BrownWaterRats attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ _ -> do
      check attrs []
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifier attrs eid CannotTakeDamage
      pure e
    _ | Just eid <- leaving, eid /= attrs.id -> do
      stays <- elem CannotLeavePlay <$> getModifiers eid
      unless stays $ check attrs [eid]
      BrownWaterRats <$> runMessage msg attrs
    _ -> BrownWaterRats <$> runMessage msg attrs
   where
    leaving = case msg of
      DoStep 1 (Destroy _ (EnemyTarget eid)) -> Just eid
      _ -> removedEnemy msg
