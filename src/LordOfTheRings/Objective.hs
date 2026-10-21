{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Objective (module LordOfTheRings.Objective, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id
import LordOfTheRings.Json (withDefaults)
import LordOfTheRings.Objective.Cards.DungeonTorch
import LordOfTheRings.Objective.Cards.GandalfsMap
import LordOfTheRings.Objective.Cards.ShadowKey
import LordOfTheRings.Objective.Cards.SignsOfGollum
import LordOfTheRings.Objective.Cards.GrimbeornTheOld
import LordOfTheRings.Objective.Cards.Athelas
import LordOfTheRings.Objective.Cards.Wilyador
import LordOfTheRings.Objective.Cards.GollumTheDeadMarshes
import LordOfTheRings.Objective.Runner ()
import LordOfTheRings.Objective.Types as X
import LordOfTheRings.Prelude

-- | A stored objective is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Objective where
  parseJSON v = do
    attrs <- parseJSON @ObjectiveAttrs (withDefaults [("guard", Null)] v)
    pure $ overAttrs (const attrs) $ lookupObjective (objectiveCardCode attrs) (objectiveCardId attrs) (objectiveId attrs)

instance RunMessage Objective where
  runMessage msg (Objective a) = Objective <$> runMessage msg a

createObjective :: EncounterCard -> ObjectiveId -> Objective
createObjective card = lookupObjective (toCardCode card) (ecId card)

lookupObjective :: (HasCallStack) => CardCode -> CardId -> ObjectiveId -> Objective
lookupObjective cardCode cardId ident = case Map.lookup cardCode allObjectives of
  Just (SomeObjectiveCard builder) -> Objective $ cbCardBuilder builder cardId ident
  Nothing -> error $ "Unknown objective: " <> show cardCode

newObjective :: (MonadRandom m) => EncounterCard -> m Objective
newObjective card = createObjective card <$> getRandom

allObjectives :: Map CardCode SomeObjectiveCard
allObjectives =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeObjectiveCard builder) <- [SomeObjectiveCard gandalfsMap, SomeObjectiveCard dungeonTorch, SomeObjectiveCard shadowKey, SomeObjectiveCard signsOfGollum, SomeObjectiveCard grimbeornTheOld, SomeObjectiveCard athelas, SomeObjectiveCard wilyador, SomeObjectiveCard gollumTheDeadMarshes]
    ]
