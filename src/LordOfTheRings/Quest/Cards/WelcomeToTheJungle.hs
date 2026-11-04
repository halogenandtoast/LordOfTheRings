module LordOfTheRings.Quest.Cards.WelcomeToTheJungle (welcomeToTheJungle) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (wildMumak)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))

newtype WelcomeToTheJungle = WelcomeToTheJungle QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

welcomeToTheJungle :: QuestCard WelcomeToTheJungle
welcomeToTheJungle = quest WelcomeToTheJungle Cards.welcomeToTheJungle

-- | The players cannot advance unless the number of Wild Mûmak in play is equal to the
-- number of players: neither fewer nor more, as when a player is eliminated with his
-- Mûmak still in play, until one is destroyed.
instance HasModifiersFor WelcomeToTheJungle where
  getModifiersFor (WelcomeToTheJungle a) = when a.flipped do
    mumaks <- length <$> select wildMumak
    players <- length <$> getRemainingPlayers
    when (mumaks /= players) $ modifySelf a [CannotBeDefeated]

-- | Forced: After the active location leaves play as an explored location, shuffle 1 set
-- aside Wild Mûmak into the encounter deck.
instance HasAbilities WelcomeToTheJungle where
  getAbilities (WelcomeToTheJungle a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow When | a.flipped]

{- | Setup: Set each copy of Wild Mûmak and each Capture objective aside, out of play. Each
player adds 1 different location to the staging area. Shuffle the encounter deck.
The scenario set them aside as it gathered the encounter deck. Each player searches in
turn, from the first player, for a location unlike those already in the staging area, as
Stop the War Party's.
-}
instance RunMessage WelcomeToTheJungle where
  runMessage msg q@(WelcomeToTheJungle attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      doStep 1 msg
      pure $ WelcomeToTheJungle $ attrs {questFlipped = True}
    DoStep n (Flip (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          staged <-
            getsGame
              (map (CardWithCode . toCardCode) . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
          searchEncounterDeckToStaging pid (CardWithType LocationType <> NotCard (CardMatchAny staged))
          doStep (n + 1) (Flip $ toTarget attrs)
        [] -> shuffleEncounterDeck
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      shuffleSetAsideIntoEncounterDeck $ take 1 $ filter (cardMatches $ cardIs Enemies.wildMumak) aside
      pure q
    _ -> WelcomeToTheJungle <$> runMessage msg attrs
