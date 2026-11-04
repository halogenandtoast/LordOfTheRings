module LordOfTheRings.Enemy.Cards.UrukSoldierOrcsOfMordor (urukSoldierOrcsOfMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype UrukSoldierOrcsOfMordor = UrukSoldierOrcsOfMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukSoldierOrcsOfMordor :: EnemyCard UrukSoldierOrcsOfMordor
urukSoldierOrcsOfMordor = enemy UrukSoldierOrcsOfMordor Cards.urukSoldierOrcsOfMordor

-- | Forced: After Uruk Soldier engages you, raise your threat by 2.
instance HasAbilities UrukSoldierOrcsOfMordor where
  getAbilities (UrukSoldierOrcsOfMordor a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, return
-- attacking enemy to the staging area.
instance RunMessage UrukSoldierOrcsOfMordor where
  runMessage msg e@(UrukSoldierOrcsOfMordor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> encounterRaisesThreat pid 2
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      stillHere <- selectAny (EnemyWithId eid <> not_ EnemyInStagingArea)
      when stillHere $ placeEnemy eid StagingArea
      pure e
    _ -> UrukSoldierOrcsOfMordor <$> runMessage msg attrs
