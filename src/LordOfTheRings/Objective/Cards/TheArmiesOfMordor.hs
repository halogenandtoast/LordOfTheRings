module LordOfTheRings.Objective.Cards.TheArmiesOfMordor (theArmiesOfMordor) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

-- | Of the Power of Mordor deck (The Temple of Doom, The Fortress of Nurn).
newtype TheArmiesOfMordor = TheArmiesOfMordor ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theArmiesOfMordor :: ObjectiveCard TheArmiesOfMordor
theArmiesOfMordor = objective TheArmiesOfMordor Cards.theArmiesOfMordor

-- | Forced: After a non-unique enemy engages a player, that enemy cannot take damage until
-- the end of the round.
instance HasAbilities TheArmiesOfMordor where
  getAbilities (TheArmiesOfMordor a) = [mkAbility a 1 $ forced $ MatchingEnemyEngagedWindow After (NotEnemy UniqueEnemy)]

instance RunMessage TheArmiesOfMordor where
  runMessage msg o@(TheArmiesOfMordor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [eid | Window _ (EnemyEngaged eid _) <- windows] \eid -> roundModifier attrs eid CannotTakeDamage
      pure o
    _ -> TheArmiesOfMordor <$> runMessage msg attrs
