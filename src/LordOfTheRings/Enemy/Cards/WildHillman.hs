module LordOfTheRings.Enemy.Cards.WildHillman (wildHillman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WildHillman = WildHillman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wildHillman :: EnemyCard WildHillman
wildHillman = enemy WildHillman Cards.wildHillman

-- | Forced: After Wild Hillman attacks, place 1 progress on the active location.
instance HasAbilities WildHillman where
  getAbilities (WildHillman a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

{- | Shadow: Place 1 progress on the active location. Attacking enemy gets +1 attack for
each progress on the active location: counted once the progress is placed.
-}
instance RunMessage WildHillman where
  runMessage msg e@(WildHillman attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      traverse_ (\l -> placeProgress attrs l 1) =<< select IsActiveLocation
      pure e
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      traverse_ (\l -> placeProgress (CardIdSource cid) l 1) =<< select IsActiveLocation
      doStep 1 msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      progress <- sum . map (.progress) <$> (traverse getLocation =<< select IsActiveLocation)
      when (progress > 0) $ enemyGetsAttack (CardIdSource cid) eid progress
      pure e
    _ -> WildHillman <$> runMessage msg attrs
