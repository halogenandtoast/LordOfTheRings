module LordOfTheRings.Quest.Cards.TheGreatRiver (theGreatRiver) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheGreatRiver = TheGreatRiver QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGreatRiver :: QuestCard TheGreatRiver
theGreatRiver = quest TheGreatRiver Cards.theGreatRiver

-- | Enemies get +2 defense and cannot be engaged. Skip the encounter phase. The players
-- cannot advance while The Argonath is in play.
instance HasModifiersFor TheGreatRiver where
  getModifiersFor (TheGreatRiver a) = when a.flipped do
    players <- getPlayerOrder
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e (DefenseModifier 2 : map CannotEngagePlayer players)
    modified a GameTarget [SkipPhase EncounterPhase]
    argonath <- selectAny (LocationWithCardCode $ toCardCode Locations.theArgonath)
    when argonath $ modifySelf a [CannotBeDefeated]

-- | Setup: Set Frodo's Choice, Seat of Seeing, and Parth Galen aside, out of play (the
-- scenario does, as it gathers the encounter deck). Add The Argonath and Sarn Gebir to the
-- staging area.
instance RunMessage TheGreatRiver where
  runMessage msg q@(TheGreatRiver attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.theArgonath)
      addSetAsideToStaging (cardIs Locations.sarnGebir)
      TheGreatRiver <$> runMessage msg attrs
    _ -> TheGreatRiver <$> runMessage msg attrs
