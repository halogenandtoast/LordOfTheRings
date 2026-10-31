module LordOfTheRings.Quest.Cards.ThePursuit (thePursuit) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ThePursuit = ThePursuit QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thePursuit :: QuestCard ThePursuit
thePursuit = quest ThePursuit Cards.thePursuit

-- | Setup: Search the encounter deck for Drû-buri-Drû and set him aside, out of play.
-- Shuffle the encounter deck.
-- When Revealed: Reveal 1 card from the encounter deck per player and add it to the
-- staging area.
instance RunMessage ThePursuit where
  runMessage msg q@(ThePursuit attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      -- the nightmare Drû-buri-Drû in place of the standard one
      let dru = map toCardCode [Enemies.druBuriDru, Enemies.druBuriDruNightmare]
      traverse_ setAside $ find ((`elem` dru) . toCardCode) deck
      shuffleEncounterDeck
      revealEncounterCards . length =<< getRemainingPlayers
      pure $ ThePursuit $ attrs {questFlipped = True}
    _ -> ThePursuit <$> runMessage msg attrs
