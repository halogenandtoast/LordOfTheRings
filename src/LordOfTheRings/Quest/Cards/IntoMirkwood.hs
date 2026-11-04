module LordOfTheRings.Quest.Cards.IntoMirkwood (intoMirkwood) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (genPlayerCard)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (unconsciousRestrictions)

newtype IntoMirkwood = IntoMirkwood QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoMirkwood :: QuestCard IntoMirkwood
intoMirkwood = quest IntoMirkwood Cards.intoMirkwood

-- | Unconscious characters cannot quest, attack, defend, collect resources, trigger
-- abilities, be poisoned, or ready (except by effects that target unconscious characters).
instance HasModifiersFor IntoMirkwood where
  getModifiersFor (IntoMirkwood a) = when a.flipped $ unconsciousRestrictions a

{- | Setup: Each player may shuffle 1 copy of Bilbo's Magic Ring into his deck. Search the
encounter deck for The Spider's Glade and set it aside, out of play. Then, shuffle the
encounter deck and reveal 1 encounter card per player. (The scenario sets The Spider's
Glade aside as it gathers.)
-}
instance RunMessage IntoMirkwood where
  runMessage msg q@(IntoMirkwood attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        ring <- genPlayerCard Attachments.bilbosMagicRing (Just pid)
        chooseOneM pid do
          questionLabeled "intoMirkwood.magicRing"
          labeled "intoMirkwood.shuffleIn" $ shuffleIntoDeck pid ring
          labeled "intoMirkwood.leaveOut" $ pure ()
      shuffleEncounterDeck
      eachPlayer $ const revealEncounterCard
      IntoMirkwood <$> runMessage msg attrs
    _ -> IntoMirkwood <$> runMessage msg attrs
