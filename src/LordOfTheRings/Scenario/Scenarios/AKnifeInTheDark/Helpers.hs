-- | What A Knife in the Dark's cards share: the Ringwraiths stage 1A sets aside, out of
-- play, which its cards shuffle back into the encounter deck.
module LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark.Helpers (
  eitherRingwraithOr,
) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Target

{- | "Either shuffle 1 out of play Ringwraith into the encounter deck or ...": the player
chooses. An option is offered only if it can be done in full, the other then being a must;
with neither, both are (FAQ 1.44, "must either X or Y"). The Ringwraiths out of play are
those stage 1A set aside.
-}
eitherRingwraithOr
  :: (ReverseQueue m, Targetable source)
  => PlayerId -> source -> Text -> Bool -> Text -> QueueT Message m () -> m ()
eitherRingwraithOr pid source question able other action = do
  ringwraiths <- getSetAside (cardIs Enemies.ringwraith)
  chooseOrRunOneM pid do
    questionLabeled question
    causedBy source
    for_ (take 1 ringwraiths) \c -> labeled "aKnifeInTheDark.shuffleRingwraith" $ shuffleSetAsideIntoEncounterDeck [c]
    when (able || null ringwraiths) $ labeled other action
