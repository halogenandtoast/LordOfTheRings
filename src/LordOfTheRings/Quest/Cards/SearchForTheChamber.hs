module LordOfTheRings.Quest.Cards.SearchForTheChamber (searchForTheChamber) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype SearchForTheChamber = SearchForTheChamber QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

searchForTheChamber :: QuestCard SearchForTheChamber
searchForTheChamber = quest SearchForTheChamber Cards.searchForTheChamber

-- | Setup: Search the encounter deck for Book of Mazarbul, and have the first player attach
-- it to a hero of his choice. Shuffle the encounter deck.
-- When Revealed: Reveal 1 encounter card per player, and add it to the staging area.
instance RunMessage SearchForTheChamber where
  runMessage msg q@(SearchForTheChamber attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      lead <- getLead
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (find ((== toCardCode Objectives.bookOfMazarbul) . toCardCode) deck) \book -> do
        removeFromEncounterDeck book
        heroes <- select AnyHero
        chooseOrRunOneM lead do
          questionLabeled "searchForTheChamber.attach"
          causedBy attrs
          targets heroes (putObjectiveIntoPlayAttached book)
      shuffleEncounterDeck
      eachPlayer $ const revealEncounterCard
      pure $ SearchForTheChamber $ attrs {questFlipped = True}
    _ -> SearchForTheChamber <$> runMessage msg attrs
