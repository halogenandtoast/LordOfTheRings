module LordOfTheRings.Objective.Cards.StalkingTheRuins (stalkingTheRuins) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers (summonNaurlhug)

newtype StalkingTheRuins = StalkingTheRuins ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stalkingTheRuins :: ObjectiveCard StalkingTheRuins
stalkingTheRuins = objective StalkingTheRuins Cards.stalkingTheRuins

instance HasAbilities StalkingTheRuins where
  getAbilities (StalkingTheRuins a) =
    [ mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase
    , mkAbility a 2 $ forced $ AdvancedToStageWindow When
    ]

instance RunMessage StalkingTheRuins where
  runMessage msg o@(StalkingTheRuins a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeTokens a a Resource 1
      doStep 1 msg
      pure o
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      when (a.resources >= 3) summonNaurlhug
      pure o
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      for_ (Map.toList a.objectiveTokens) \(token, n) -> removeTokens a a token n
      dragons <- select $ EnemyWithCardCode (toCardCode Enemies.naurlhug)
      traverse_ setAsideEnemy dragons
      pure o
    _ -> StalkingTheRuins <$> runMessage msg a
