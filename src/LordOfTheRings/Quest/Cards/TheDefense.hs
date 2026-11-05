module LordOfTheRings.Quest.Cards.TheDefense (theDefense) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (lowestThreatBattleground)
import LordOfTheRings.Trait (Trait (Battleground))

newtype TheDefense = TheDefense QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDefense :: QuestCard TheDefense
theDefense = quest TheDefense Cards.theDefense

-- | Players must deal damage from undefended attacks to the lowest threat Battleground
-- location in play.
instance HasModifiersFor TheDefense where
  getModifiersFor (TheDefense a) = when a.flipped do
    lowest <- lowestThreatBattleground
    players <- getRemainingPlayers
    for_ lowest \l -> for_ players \p -> modified a (PlayerTarget p) [UndefendedDamageTo (toTarget l)]

-- | Setup: Add The Approach, The Citadel, and The Banks to the staging area. Shuffle the
-- encounter deck.
-- If there are no Battleground locations in play, immediately advance to the next stage.
instance RunMessage TheDefense where
  runMessage msg q@(TheDefense attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let codes = map toCardCode [Locations.theApproach, Locations.theCitadel, Locations.theBanks]
      for_ (filter ((`elem` codes) . toCardCode) deck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      shuffleEncounterDeck
      pure $ TheDefense $ attrs {questFlipped = True}
    CheckStage qid | qid == attrs.id -> do
      whenM (selectNone $ LocationWithTrait Battleground) $ push (CompleteQuest attrs.id)
      pure q
    RemoveFromGame (LocationTarget _) -> recheck
    AddToVictoryDisplay (LocationTarget _) -> recheck
    Discard _ (LocationTarget _) -> recheck
    _ -> TheDefense <$> runMessage msg attrs
   where
    recheck = do
      checkStage attrs.id
      TheDefense <$> runMessage msg attrs
