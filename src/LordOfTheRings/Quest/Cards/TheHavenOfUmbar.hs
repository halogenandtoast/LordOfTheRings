module LordOfTheRings.Quest.Cards.TheHavenOfUmbar (theHavenOfUmbar) where

import LordOfTheRings.Card (CardType (EnemyType, LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfCorsairs.Helpers (stormcallerDestroyed)
import LordOfTheRings.Trait (Trait (Raider))

newtype TheHavenOfUmbar = TheHavenOfUmbar QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHavenOfUmbar :: QuestCard TheHavenOfUmbar
theHavenOfUmbar = quest TheHavenOfUmbar Cards.theHavenOfUmbar

-- | When Revealed: Search the encounter deck for The Shattered Monument and Captain Sahír,
-- and set them aside, out of play. Each player searches the encounter deck for 1 enemy and
-- 1 location, and adds them to the staging area. If the Stormcaller was not destroyed
-- during stage 1, add 1 resource to each Raider enemy in play.
instance RunMessage TheHavenOfUmbar where
  runMessage msg q@(TheHavenOfUmbar attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheHavenOfUmbar <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      -- Nightmare: "also search the encounter deck for The Cursed Sword and attach it to
      -- the set-aside Captain Sahír": set aside with him, it is attached as he enters play
      nightmare <- inNightmareMode
      for_ ([Locations.theShatteredMonument, Enemies.captainSahirTheCityOfCorsairs] <> [Objectives.theCursedSword | nightmare]) \def ->
        traverse_ setAside $ take 1 $ filter (cardMatches $ cardIs def) deck
      eachPlayer \p -> do
        searchEncounterDeckToStaging p (CardWithType EnemyType)
        searchEncounterDeckToStaging p (CardWithType LocationType)
      doStep 1 msg
      pure q
    -- the Raiders counted once the players have added theirs
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      unlessM stormcallerDestroyed do
        raiders <- select (EnemyWithTrait Raider)
        for_ raiders \r -> placeResources attrs r 1
      pure q
    _ -> TheHavenOfUmbar <$> runMessage msg attrs
