module LordOfTheRings.Location.Cards.TheSecretChamber (theSecretChamber) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (currentQuestProgress)

newtype TheSecretChamber = TheSecretChamber LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSecretChamber :: LocationCard TheSecretChamber
theSecretChamber = location TheSecretChamber Cards.theSecretChamber

-- | X is the number of players in the game. Immune to player card effects.
-- Travel: Remove 3 progress from the current quest to travel here.
instance HasModifiersFor TheSecretChamber where
  getModifiersFor (TheSecretChamber a) = do
    players <- length <$> getRemainingPlayers
    progress <- currentQuestProgress
    modified a a $ [ThreatModifier players, ImmuneToPlayerCardEffects] <> [CannotBeTraveledTo | progress < 3]

-- | If The Secret Chamber is placed underneath The Orcs' Search, the players lose the game.
instance RunMessage TheSecretChamber where
  runMessage msg l@(TheSecretChamber a) = case msg of
    TravelTo lid | lid == a.id -> do
      removeProgressFromQuest a 3
      pure l
    PlaceUnderneath _ cards | a.cardId `elem` map (.id) cards -> do
      gameOver Lost
      pure l
    _ -> TheSecretChamber <$> runMessage msg a
