module LordOfTheRings.Enemy.Cards.RottingThing (rottingThing) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (engagedPlayerCannotReduceThreat)

newtype RottingThing = RottingThing EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

rottingThing :: EnemyCard RottingThing
rottingThing = enemy RottingThing Cards.rottingThing

instance HasModifiersFor RottingThing where
  getModifiersFor (RottingThing a) = do
    modifySelf a [CannotHaveAttachments]
    engagedPlayerCannotReduceThreat a

instance HasAbilities RottingThing where
  getAbilities (RottingThing a) = [mkAbility a 1 $ forced $ EnemyDestroyedCharacterWithExcessDamageWindow After a.id]

instance RunMessage RottingThing where
  runMessage msg e@(RottingThing a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [n | Window _ (AttackDealtExcessDamage eid _ n) <- ws, eid == a.id] \n -> do
        locations <- select IsActiveLocation
        for_ locations \lid -> placeResources a lid n
      pure e
    _ -> RottingThing <$> runMessage msg a
