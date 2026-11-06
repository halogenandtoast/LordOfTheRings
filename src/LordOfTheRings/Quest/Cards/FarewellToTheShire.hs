module LordOfTheRings.Quest.Cards.FarewellToTheShire (farewellToTheShire) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers (advanceForestStage)

newtype FarewellToTheShire = FarewellToTheShire QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

farewellToTheShire :: QuestCard FarewellToTheShire
farewellToTheShire = quest FarewellToTheShire Cards.farewellToTheShire

instance RunMessage FarewellToTheShire where
  runMessage msg q@(FarewellToTheShire a) = case msg of
    DoStep 7 Setup -> q <$ askToPerformSetup a
    PerformSetup (isTarget a -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (filter (\card -> toCardCode card `elem` map toCardCode [Enemies.oldManWillow, Locations.withywindle]) deck) setAside
      order <- getPlayerOrder
      searchDifferentEncounterCards order a (CardWithType LocationType) False
      turnOver a
      pure q
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      FarewellToTheShire <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> drawCards a pid 1
      advanceForestStage a False
      pure q
    _ -> FarewellToTheShire <$> runMessage msg a
