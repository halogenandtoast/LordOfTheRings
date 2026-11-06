module LordOfTheRings.Location.Cards.DeepGulley (deepGulley) where

import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DeepGulley = DeepGulley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deepGulley :: LocationCard DeepGulley
deepGulley = location DeepGulley Cards.deepGulley

instance HasAbilities DeepGulley where
  getAbilities (DeepGulley a) = [mkAbility a 1 $ forced $ AnyOptionallyEngagedWindow After | a.placement == StagingArea]

instance RunMessage DeepGulley where
  runMessage msg l@(DeepGulley a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [(eid, pid) | Window _ (OptionallyEngaged eid pid) <- windows] \(eid, pid) ->
        encounterRaisesThreat pid =<< getEnemyAttack eid
      pure l
    _ -> DeepGulley <$> runMessage msg a
