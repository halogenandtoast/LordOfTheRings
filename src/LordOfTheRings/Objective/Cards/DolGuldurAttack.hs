module LordOfTheRings.Objective.Cards.DolGuldurAttack (dolGuldurAttack) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Trait (Trait (Lieutenant))

newtype DolGuldurAttack = DolGuldurAttack ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dolGuldurAttack :: ObjectiveCard DolGuldurAttack
dolGuldurAttack = objective DolGuldurAttack Cards.dolGuldurAttack

instance HasAbilities DolGuldurAttack where
  -- The printed Forced is explicitly after every stage 3 Forced effect,
  -- so its window is opened by the scenario after those effects finish.
  getAbilities (DolGuldurAttack a) = [mkAbility a 1 $ forced DolGuldurStageEffectsResolvedWindow]

setStrength :: Int -> ObjectiveAttrs -> ObjectiveAttrs
setStrength amount a = a & tokensL %~ Map.insert Resource (max 0 amount)

instance RunMessage DolGuldurAttack where
  runMessage msg o@(DolGuldurAttack a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      push $ ScenarioSpecific "dolGuldur.resolveSiege" Null
      replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      when (isNothing replica) $ push $ CheckWindows [mkWhen DolGuldurSiegeResolved]
      pure o
    DoStep 1 (PlaceTokens _ (isTarget a -> True) Resource amount) -> do
      shared <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      pure $ DolGuldurAttack $ setStrength (fromMaybe amount $ shared >>= dolGuldurCityStrength . epicShared) a
    PlaceTokens _ (isTarget a -> True) Damage amount -> change $ negate amount
    PlaceTokens _ (isTarget a -> True) Resource amount -> change amount
    RemoveTokens _ (isTarget a -> True) Resource amount -> change $ negate amount
    SyncEpic shared | Just strength <- dolGuldurCityStrength shared -> do
      when (strength == 0) checkLieutenants
      pure $ DolGuldurAttack $ setStrength strength a
    ContributeShared DolGuldurCityStrength amount -> do
      replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      let strength = do
            shared <- epicShared <$> replica
            current <- dolGuldurCityStrength shared
            pure $ if isJust (dolGuldurOutcome shared) then current else max 0 (current + amount)
      when (strength == Just 0) checkLieutenants
      pure $ maybe o (DolGuldurAttack . (`setStrength` a)) strength
    _ -> DolGuldurAttack <$> runMessage msg a
   where
    change amount = do
      replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      if isJust (replica >>= dolGuldurCityStrength . epicShared)
        then o <$ push (ContributeShared DolGuldurCityStrength amount)
        else do
          when (a.resources + amount <= 0) checkLieutenants
          pure $ DolGuldurAttack $ setStrength (a.resources + amount) a
    checkLieutenants = do
      foes <- select $ EnemyWithTrait Lieutenant
      traverse_ (checkDefeated a) foes
