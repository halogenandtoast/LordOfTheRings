module LordOfTheRings.Quest.Cards.TheDisappearance (theDisappearance) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheDisappearance = TheDisappearance QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDisappearance :: QuestCard TheDisappearance
theDisappearance = quest TheDisappearance Cards.theDisappearance

instance HasModifiersFor TheDisappearance where
  getModifiersFor (TheDisappearance a) = when a.flipped do
    blocked <- selectAny $ LocationWithCardCode (toCardCode Locations.tarlangsNeck)
    when blocked $ modifySelf a [CannotBeDefeated]

instance RunMessage TheDisappearance where
  runMessage msg q@(TheDisappearance a) = case msg of
    DoStep 7 Setup -> q <$ askToPerformSetup a
    PerformSetup (isTarget a -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (find (cardMatches $ cardIs Locations.tarlangsNeck) deck) \card -> do
        removeFromEncounterDeck card
        putIntoPlayAsActiveLocation card
      let aside = [Objectives.derufin, Enemies.theLordOfTheDeadErech, Locations.theStoneOfErechStandalone, Objectives.eventide, Objectives.dusk, Objectives.midnight]
      for_ (filter (\card -> any (\def -> cardMatches (cardIs def) card) aside) deck) setAside
      shuffleEncounterDeck
      addSetAsideToStaging (cardIs Objectives.eventide)
      turnOver a
      pure q
    Flip (isTarget a -> True) | not a.flipped -> do
      askToResolveWhenRevealed a
      TheDisappearance <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      revealEncounterCards . length =<< getPlayerOrder
      pure q
    _ -> TheDisappearance <$> runMessage msg a
