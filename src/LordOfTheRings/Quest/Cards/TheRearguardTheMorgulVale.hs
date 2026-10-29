module LordOfTheRings.Quest.Cards.TheRearguardTheMorgulVale (theRearguardTheMorgulVale) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (captainCannotLeavePlay, isCaptain)

newtype TheRearguardTheMorgulVale = TheRearguardTheMorgulVale QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRearguardTheMorgulVale :: QuestCard TheRearguardTheMorgulVale
theRearguardTheMorgulVale = quest TheRearguardTheMorgulVale Cards.theRearguardTheMorgulVale

-- | Murzag cannot leave play unless destroyed.
instance HasModifiersFor TheRearguardTheMorgulVale where
  getModifiersFor (TheRearguardTheMorgulVale a) = captainCannotLeavePlay a Enemies.murzag

-- | Setup: Set aside Lord Alcaron, Nazgûl of Minas Morgul, and The White Bridge. Add To the
-- Tower, Murzag and 1 copy of Morgul Vale to the staging area.
-- If Murzag is defeated, advance to the next stage.
instance RunMessage TheRearguardTheMorgulVale where
  runMessage msg q@(TheRearguardTheMorgulVale attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let copies def = filter ((== toCardCode def) . toCardCode) deck
      traverse_ setAside $ concatMap copies [Enemies.lordAlcaron, Enemies.nazgulOfMinasMorgul, Locations.theWhiteBridge]
      for_ (concatMap (take 1 . copies) [Objectives.toTheTower, Enemies.murzag, Locations.morgulVale]) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      pure $ TheRearguardTheMorgulVale $ attrs {questFlipped = True}
    Destroy _ (EnemyTarget eid) -> do
      whenM (isCaptain Enemies.murzag eid) $ completeQuest attrs.id
      TheRearguardTheMorgulVale <$> runMessage msg attrs
    _ -> TheRearguardTheMorgulVale <$> runMessage msg attrs
