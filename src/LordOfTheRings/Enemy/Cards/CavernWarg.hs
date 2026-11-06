module LordOfTheRings.Enemy.Cards.CavernWarg (cavernWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (readyCaveTorch)

newtype CavernWarg = CavernWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cavernWarg :: EnemyCard CavernWarg
cavernWarg = enemy CavernWarg Cards.cavernWarg

instance HasAbilities CavernWarg where
  getAbilities (CavernWarg a) =
    [ restricted a 1 (ReadyObjectiveWithCardCode $ toCardCode Objectives.caveTorchDarkOfMirkwood)
        $ response $ EnemyAttacksWindow When a.id
    ]

instance RunMessage CavernWarg where
  runMessage msg e@(CavernWarg a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      readyCaveTorch >>= traverse_ \(torch, _) -> do
        exhaust torch
        cancelAttack a.id
        placeEnemy a.id StagingArea
      pure e
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      placeEnemy eid StagingArea
      pure e
    _ -> CavernWarg <$> runMessage msg a
