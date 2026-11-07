module LordOfTheRings.Objective.Cards.Annuminas (annuminas) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Types (scenarioEpic)

newtype Annuminas = Annuminas ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

annuminas :: ObjectiveCard Annuminas
annuminas = objective Annuminas Cards.annuminas

instance HasAbilities Annuminas where
  getAbilities (Annuminas a) = [mkAbility a 1 $ forced RoundEndsWindow]

setStrength :: Int -> ObjectiveAttrs -> ObjectiveAttrs
setStrength amount a = a & tokensL %~ Map.insert Resource (max 0 amount)

instance RunMessage Annuminas where
  runMessage msg o@(Annuminas a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      enemies <- length <$> select AnyEnemy
      replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      let remote = sum
            [ length $ filter ((== EpicEnemy) . epicTargetKind) board.epicBoardTargets
            | r <- maybeToList replica, board <- r.epicBoards, board.epicBoardGroup /= r.epicGroup
            ]
      dealDamage a a (enemies + remote)
      pure o
    -- Setup sets a value; ordinary resource placement is a repair. An Epic
    -- city mirrors the event's initial fifty rather than contributing it again.
    DoStep 1 (PlaceTokens _ (isTarget a -> True) Resource amount) -> do
      shared <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      let strength = fromMaybe amount (shared >>= annuminasCityStrength . epicShared)
      when (strength <= 0) $ gameOver Lost
      pure $ Annuminas $ setStrength strength a
    PlaceTokens _ (isTarget a -> True) Damage amount -> change (negate amount)
    PlaceTokens _ (isTarget a -> True) Resource amount -> change amount
    RemoveTokens _ (isTarget a -> True) Resource amount -> change (negate amount)
    SyncEpic shared | Just strength <- annuminasCityStrength shared -> pure $ Annuminas $ setStrength strength a
    ContributeShared AnnuminasCityStrength amount -> do
      replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      let strength = do
            shared <- epicShared <$> replica
            current <- annuminasCityStrength shared
            pure $ if isJust (annuminasOutcome shared) then current else max 0 (current + amount)
      pure $ maybe o (Annuminas . (`setStrength` a)) strength
    _ -> Annuminas <$> runMessage msg a
   where
    change amount = do
      replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      if isJust (replica >>= annuminasCityStrength . epicShared)
        then o <$ push (ContributeShared AnnuminasCityStrength amount)
        else do
          let strength = max 0 $ a.resources + amount
          when (strength == 0) $ gameOver Lost
          pure $ Annuminas $ setStrength strength a
