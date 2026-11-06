module LordOfTheRings.Quest.Cards.BeyondExpectations (beyondExpectations) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (WestBank))

newtype BeyondExpectations = BeyondExpectations QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

beyondExpectations :: QuestCard BeyondExpectations
beyondExpectations = quest BeyondExpectations Cards.beyondExpectations

instance HasModifiersFor BeyondExpectations where
  getModifiersFor (BeyondExpectations a) = when a.flipped do
    west <- select (LocationWithTrait WestBank)
    for_ west \lid -> modified a lid [CannotBeTraveledTo]

instance RunMessage BeyondExpectations where
  runMessage msg q@(BeyondExpectations attrs) = case msg of
    DoStep 7 Setup -> askToPerformSetup attrs >> pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getPlayerOrder
      for_ [Enemies.snagaScouts, Enemies.wolvesFromMordor, Enemies.wainriders] \def ->
        for_ (take players $ filter (cardMatches $ cardIs def) deck) \card -> do
          removeFromEncounterDeck card
          addToStaging card
      traverse_ setAside $ find (cardMatches $ cardIs Enemies.theWitchKingOsgiliath) deck
      shuffleEncounterDeck
      turnOver attrs
      pure q
    _ -> BeyondExpectations <$> runMessage msg attrs
