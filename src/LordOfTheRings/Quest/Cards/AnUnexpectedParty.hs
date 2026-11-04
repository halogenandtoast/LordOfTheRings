module LordOfTheRings.Quest.Cards.AnUnexpectedParty (anUnexpectedParty) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AnUnexpectedParty = AnUnexpectedParty QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

anUnexpectedParty :: QuestCard AnUnexpectedParty
anUnexpectedParty = quest AnUnexpectedParty Cards.anUnexpectedParty

{- | Setup: Remove and shuffle the 7 Sack cards into a Sack deck and set it aside face down.
Remove the 3 Troll enemies and the Troll Cave from the encounter deck and set them aside out
of play. Then, shuffle the encounter deck. Each player reveals 1 card from the top of the
encounter deck and adds it to the staging area.

The scenario builds the sack deck and sets the Trolls and Troll Cave aside as it gathers
the encounter deck, which is then shuffled.
-}
instance RunMessage AnUnexpectedParty where
  runMessage msg q@(AnUnexpectedParty attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      eachPlayer $ const revealEncounterCard
      turnOver attrs
      pure q
    _ -> AnUnexpectedParty <$> runMessage msg attrs
