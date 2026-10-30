module LordOfTheRings.Quest.Cards.SecretsOfRhudaur (secretsOfRhudaur) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (getRhudaurSideQuests)

newtype SecretsOfRhudaur = SecretsOfRhudaur QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

secretsOfRhudaur :: QuestCard SecretsOfRhudaur
secretsOfRhudaur = quest SecretsOfRhudaur Cards.secretsOfRhudaur

-- | During the quest phase, Secrets of Rhudaur cannot be chosen as the current quest.
instance HasModifiersFor SecretsOfRhudaur where
  getModifiersFor (SecretsOfRhudaur a) = modified a a [CannotBeCurrentQuest]

-- | Time 5. Forced: After the last time counter is removed from Secrets of Rhudaur, remove
-- each Treachery of Rhudaur side quest from the game and immediately advance to stage 2.
instance HasAbilities SecretsOfRhudaur where
  getAbilities (SecretsOfRhudaur a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Ancient Causeway, of Ruins of Arnor.
ancientCauseway :: CardMatcher
ancientCauseway = CardWithCode "09057"

-- | Setup: Make The Great Hall the active location. Set Thaurdir aside, out of play. Add
-- the 3 Treachery of Rhudaur side quests to the staging area, quest side faceup. Search the
-- encounter deck for 1 copy of Ancient Causeway, and add it to the staging area (2 copies
-- instead if there are 3 or 4 players in the game). The first player gains control of
-- Amarthiúl.
-- If there are no Treachery of Rhudaur side quests in play, immediately advance to stage 2.
instance RunMessage SecretsOfRhudaur where
  runMessage msg q@(SecretsOfRhudaur attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getRemainingPlayers
      let copies m = filter (cardMatches m) deck
          stage c = removeFromEncounterDeck c >> addToStaging c
      for_ (take 1 $ copies $ cardIs Locations.theGreatHall) \card -> do
        removeFromEncounterDeck card
        putIntoPlayAsActiveLocation card
      traverse_ setAside $ copies (cardIs Enemies.thaurdir)
      traverse_ stage $ concatMap (copies . cardIs) [Cards.quietTheSpirits, Cards.siftThroughTheDebris, Cards.decipherAncientTexts]
      traverse_ stage $ take (if players >= 3 then 2 else 1) $ copies ancientCauseway
      traverse_ stage $ take 1 $ copies (cardIs Objectives.amarthiulTheTreacheryOfRhudaur)
      shuffleEncounterDeck
      pure $ SecretsOfRhudaur $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      traverse_ removeFromGame =<< getRhudaurSideQuests
      completeQuest attrs.id
      pure q
    -- a side quest defeated, or gone, may be the last
    CompleteQuest qid | qid /= attrs.id -> do
      checkStage attrs.id
      SecretsOfRhudaur <$> runMessage msg attrs
    CheckStage qid | qid == attrs.id -> do
      none <- null <$> getRhudaurSideQuests
      when none $ completeQuest attrs.id
      pure q
    _ -> SecretsOfRhudaur <$> runMessage msg attrs
