module LordOfTheRings.Location.Cards.ThroatOfTheMountain (throatOfTheMountain) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.Types (isSideQuest)

newtype ThroatOfTheMountain = ThroatOfTheMountain LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

throatOfTheMountain :: LocationCard ThroatOfTheMountain
throatOfTheMountain = location ThroatOfTheMountain Cards.throatOfTheMountain

-- | Response: When Throat of the Mountain is explored, search the quest deck for a quest
-- card and make it the current main quest. Then, flip it over. Shuffle the rest of the
-- quest deck.
instance HasAbilities ThroatOfTheMountain where
  getAbilities (ThroatOfTheMountain a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

{- | The card does not say where the main quest it replaces goes, nor do the rule sheet or
FAQ 1.9, and no ruling was found. It is not defeated, so it does not go to the victory
display (rule sheet: "When a quest is completed, players will either add it to their
victory display or win the game"). A stage that leaves un-defeated, as a bypassed one,
goes back into the quest deck side 2A up, its progress gone (rule sheet, "Bypassing a
Quest Card"); it goes back as it is replaced, before "Shuffle the rest of the quest deck",
so it is shuffled in with the rest.
-}
instance RunMessage ThroatOfTheMountain where
  runMessage msg l@(ThroatOfTheMountain a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      deck <- getsGame ((.questDeck) . toAttrs . (.scenario))
      current <- getsGame (filter (not . isSideQuest) . toList . entitiesQuests . gameEntities)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "throatOfTheMountain.quest"
        causedBy a
        showing (map EncounterCard deck)
        targets deck \chosen -> do
          for_ current shuffleIntoQuestDeck
          putQuestIntoPlayFlipped chosen
      pure l
    _ -> ThroatOfTheMountain <$> runMessage msg a
