module LordOfTheRings.Quest.Cards.AnArduousJourney (anArduousJourney) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AnArduousJourney = AnArduousJourney QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
anArduousJourney :: QuestCard AnArduousJourney
anArduousJourney = quest AnArduousJourney Cards.anArduousJourney

-- | Each player cannot ready more than 6 characters he controls during the refresh phase.
instance HasModifiersFor AnArduousJourney where
  getModifiersFor (AnArduousJourney a) = do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [RefreshAtMost 6]

-- | Response: When this stage is defeated, each player may ready a hero he controls.
instance RunMessage AnArduousJourney where
  runMessage msg (AnArduousJourney attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> do
        heroes <- select (HeroControlledBy (PlayerWithId p) <> ExhaustedHero)
        unless (null heroes) $ chooseOneM p do
          questionLabeled "anArduousJourney.ready"
          causedBy attrs
          targets heroes encounterReadies
          labeled "pass" (pure ())
      AnArduousJourney <$> runMessage msg attrs
    _ -> AnArduousJourney <$> runMessage msg attrs
