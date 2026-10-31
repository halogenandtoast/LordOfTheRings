module LordOfTheRings.Enemy.Cards.TreetopBirdEater (treetopBirdEater) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.AJourneyToRhosgobel.Helpers (getWilyador)

newtype TreetopBirdEater = TreetopBirdEater EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

treetopBirdEater :: EnemyCard TreetopBirdEater
treetopBirdEater = enemy TreetopBirdEater Cards.treetopBirdEater

-- | While Treetop Bird-eater is engaged with a player, characters controlled by that player
-- cannot be healed.
instance HasModifiersFor TreetopBirdEater where
  getModifiersFor (TreetopBirdEater a) = case a.placement of
    EngagedWith pid -> do
      characters <- charactersControlledBy pid
      for_ characters \c -> modified a c [CannotBeHealed]
    _ -> pure ()

instance HasAbilities TreetopBirdEater where
  getAbilities (TreetopBirdEater a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]

-- | Forced: At the beginning of the encounter phase, Treetop Bird-eater engages the player
-- who controls Wilyador (regardless of threat).
instance RunMessage TreetopBirdEater where
  runMessage msg e@(TreetopBirdEater attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      getWilyador >>= traverse_ \w ->
        characterController (toTarget w) >>= traverse_ \pid -> engageEnemy pid attrs.id
      pure e
    _ -> TreetopBirdEater <$> runMessage msg attrs
