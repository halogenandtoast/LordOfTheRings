{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Quest (module LordOfTheRings.Quest, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Quest.Cards.AChosenPathBeornsPath
import LordOfTheRings.Quest.Cards.AChosenPathDontLeaveThePath
import LordOfTheRings.Quest.Cards.AForkInTheRoad
import LordOfTheRings.Quest.Cards.AmbushOnTheShore
import LordOfTheRings.Quest.Cards.AnduinPassage
import LordOfTheRings.Quest.Cards.FliesAndSpiders
import LordOfTheRings.Quest.Cards.OutOfTheDungeons
import LordOfTheRings.Quest.Cards.TheNecromancersTower
import LordOfTheRings.Quest.Cards.ThroughTheCaverns
import LordOfTheRings.Quest.Cards.ToTheRiver
import LordOfTheRings.Quest.Cards.TheHuntBegins
import LordOfTheRings.Quest.Cards.ANewTerrorAbroad
import LordOfTheRings.Quest.Cards.OnTheTrail
import LordOfTheRings.Quest.Runner ()
import LordOfTheRings.Quest.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored quest is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Quest where
  parseJSON v = do
    attrs <- parseJSON @QuestAttrs v
    pure $ overAttrs (const attrs) $ lookupQuest (questCardCode attrs) (questCardId attrs) (questId attrs)

instance RunMessage Quest where
  runMessage msg (Quest a) = Quest <$> runMessage msg a

createQuest :: EncounterCard -> QuestId -> Quest
createQuest card = lookupQuest (toCardCode card) (ecId card)

lookupQuest :: (HasCallStack) => CardCode -> CardId -> QuestId -> Quest
lookupQuest cardCode cardId ident = case Map.lookup cardCode allQuests of
  Just (SomeQuestCard builder) -> Quest $ cbCardBuilder builder cardId ident
  Nothing -> error $ "Unknown quest: " <> show cardCode

newQuest :: (MonadRandom m) => EncounterCard -> m Quest
newQuest card = createQuest card <$> getRandom

allQuests :: Map CardCode SomeQuestCard
allQuests =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeQuestCard builder) <-
        [ SomeQuestCard fliesAndSpiders
        , SomeQuestCard aForkInTheRoad
        , SomeQuestCard aChosenPathDontLeaveThePath
        , SomeQuestCard aChosenPathBeornsPath
        , SomeQuestCard theNecromancersTower
        , SomeQuestCard throughTheCaverns
        , SomeQuestCard outOfTheDungeons
        , SomeQuestCard toTheRiver
        , SomeQuestCard anduinPassage
        , SomeQuestCard ambushOnTheShore
        , SomeQuestCard theHuntBegins
        , SomeQuestCard aNewTerrorAbroad
        , SomeQuestCard onTheTrail
        ]
    ]
