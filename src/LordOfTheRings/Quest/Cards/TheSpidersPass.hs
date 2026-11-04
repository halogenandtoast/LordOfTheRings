module LordOfTheRings.Quest.Cards.TheSpidersPass (theSpidersPass) where

import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (TheGreatSpider))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (shelob)
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (smeagol)

newtype TheSpidersPass = TheSpidersPass QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSpidersPass :: QuestCard TheSpidersPass
theSpidersPass = quest TheSpidersPass Cards.theSpidersPass

-- | Shelob cannot leave the staging area and Sméagol cannot attack or defend.
-- Each player cannot play, or put into play, more than 1 ally each round.
-- No more than 4 progress can be placed on this stage each round.
instance HasModifiersFor TheSpidersPass where
  getModifiersFor (TheSpidersPass a) = when a.flipped do
    traverse_ (\e -> modified a e [CannotLeaveStagingArea]) =<< select shelob
    traverse_ (\s -> modified a s [CannotAttackOrDefend]) =<< select smeagol
    traverse_ (\p -> modified a (PlayerTarget p) [OneAllyEntersPlayPerRound]) =<< getPlayerOrder
    modifySelf a [ProgressPerRoundLimit 4]

-- | When Revealed: Add Shelob to the staging area and shuffle all cards from The Great
-- Spider encounter set and the encounter discard pile into the encounter deck.
instance RunMessage TheSpidersPass where
  runMessage msg q@(TheSpidersPass attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheSpidersPass <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.shelob)
      aside <- getSetAside AnyCard
      shuffleSetAsideIntoEncounterDeck [c | c <- aside, cdEncounterSet (toCardDef c) == Just TheGreatSpider]
      shuffleEncounterDiscardIntoDeck AnyCard
      pure q
    _ -> TheSpidersPass <$> runMessage msg attrs
