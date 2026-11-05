-- | A Knife in the Dark's effects that shuffle an out-of-play Ringwraith into the encounter deck.
module LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark.Helpers (
  eitherRingwraithOr,
  shuffleOutOfPlayRingwraith,
) where

import LordOfTheRings.Card (Card (EncounterCard), EncounterCard)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Target

{- | "Either shuffle 1 out of play Ringwraith into the encounter deck or ...": the player
chooses. An option is offered only if it can be done in full, the other then being a must;
with neither, both are (FAQ 1.44, "must either X or Y"). Out-of-play cards include
both the set-aside pool and the public discard/victory areas (core rules,
"In Play and Out of Play", p. 25; FAQ 1.29).
https://images-cdn.fantasyflightgames.com/ffg_content/lotr-lcg/LOTR%20Rules.pdf
https://images-cdn.fantasyflightgames.com/filer_public/1d/bb/1dbb319c-6b1e-466e-9177-75a5857b5cfa/lotr_faq_19_printer_friendly.pdf A card already in the encounter deck cannot
be shuffled into that same area to satisfy this choice.
-}
eitherRingwraithOr
  :: (ReverseQueue m, Targetable source)
  => PlayerId -> source -> Text -> Bool -> Text -> QueueT Message m () -> m ()
eitherRingwraithOr pid source question able other action = do
  ringwraiths <- availableRingwraiths
  chooseOrRunOneM pid do
    questionLabeled question
    causedBy source
    unless (null ringwraiths)
      $ labeled "aKnifeInTheDark.shuffleRingwraith"
      $ shuffleOutOfPlayRingwraith pid source
    when (able || null ringwraiths) $ labeled other action

{- | Copies within one public area are interchangeable; the first player chooses
the area when this would leave a different pool available for later effects.
-}
availableRingwraiths :: HasGame m => m [(Text, EncounterCard)]
availableRingwraiths = do
  s <- getsGame (toAttrs . (.scenario))
  let firstWraith = take 1 . filter (cardMatches $ cardIs Enemies.ringwraith)
  pure
    $ [("setAside", c) | c <- firstWraith s.scenarioSetAside]
    <> [("discard", c) | c <- firstWraith s.encounterDiscard]
    <> [("victory", c) | c <- firstWraith [c | EncounterCard c <- s.victoryDisplay]]

shuffleOutOfPlayRingwraith :: (ReverseQueue m, Targetable source) => PlayerId -> source -> m ()
shuffleOutOfPlayRingwraith pid source = do
  available <- availableRingwraiths
  chooseOrRunOneM pid do
    questionLabeled "aKnifeInTheDark.chooseRingwraith"
    causedBy source
    for_ available \(area, card) -> labeled ("aKnifeInTheDark.ringwraithFrom." <> area) $ case area of
      "setAside" -> shuffleSetAsideIntoEncounterDeck [card]
      "discard" -> do
        removeFromEncounterDiscard card
        setAside card
        shuffleSetAsideIntoEncounterDeck [card]
      _ -> do
        removeFromVictoryDisplay (EncounterCard card)
        setAside card
        shuffleSetAsideIntoEncounterDeck [card]
