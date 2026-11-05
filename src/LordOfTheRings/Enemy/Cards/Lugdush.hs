module LordOfTheRings.Enemy.Cards.Lugdush (lugdush) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Lugdush = Lugdush EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lugdush :: EnemyCard Lugdush
lugdush = enemy Lugdush Cards.lugdush

instance HasModifiersFor Lugdush where
  getModifiersFor (Lugdush a) = whenM (selectAny $ EnemyWithId a.id <> EnemyEngagedWith AnyPlayer) do
    enemies <- select $ NotEnemy (EnemyWithId a.id)
    for_ enemies (\eid -> modified a eid [CannotTakeDamage])

instance RunMessage Lugdush where
  runMessage msg e@(Lugdush a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> UniqueEnemy) $ roundModifier a eid CannotTakeDamage
      pure e
    _ -> Lugdush <$> runMessage msg a
