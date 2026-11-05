module LordOfTheRings.Quest.Cards.TheGatesOfMoria (theGatesOfMoria) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (damageEachActiveLocation)
import LordOfTheRings.Trait (Trait (RingBearer))

newtype TheGatesOfMoria = TheGatesOfMoria QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theGatesOfMoria :: QuestCard TheGatesOfMoria
theGatesOfMoria = quest TheGatesOfMoria Cards.theGatesOfMoria

{- | There can be 2 active locations. During the travel phase, the players must travel to a
location, if able: with Doors of Durin the only active location, to one more, made active
beside it (The Road Darkens rules insert, "Stage 4").
-}
instance HasModifiersFor TheGatesOfMoria where
  getModifiersFor (TheGatesOfMoria a) = when a.flipped do
    locations <- select LocationInStagingArea
    for_ locations \l -> modified a l [MustBeTraveledTo, TravelableWithActiveLocation]

-- | Forced: After an enemy engages a player, place 1 damage on each active location.
instance HasAbilities TheGatesOfMoria where
  getAbilities (TheGatesOfMoria a) =
    [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | a.flipped]
      <> [restricted a 2 InNightmareMode $ forced $ AdvancedToStageWindow After | a.flipped]

{- | When Revealed: Make Doors of Durin the active location. Add Watcher in the Water to the
staging area. Discard all tokens from the Ring-bearer and place it (and each card attached
to it) facedown under Watcher in the Water: still in play, but no player controls him or
his attachments while he is there (rules insert). A location active before goes back to the
staging area, as one made the active location in its place does.

If Doors of Durin is explored, the players win the game.
-}
instance RunMessage TheGatesOfMoria where
  runMessage msg q@(TheGatesOfMoria attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheGatesOfMoria <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      select IsActiveLocation >>= traverse_ \l -> placeLocation l StagingArea
      traverse_ putIntoPlayAsActiveLocation
        =<< getSetAside (cardIs Locations.doorsOfDurinTheRingGoesSouth)
      watchers <- getSetAside (cardIs Enemies.watcherInTheWater)
      addSetAsideToStaging (cardIs Enemies.watcherInTheWater)
      ringBearers <- select (HeroWithTrait RingBearer)
      for_ (take 1 watchers) \watcher -> for_ ringBearers \h -> do
        hero <- getHero h
        when (hero.damage > 0) $ removeTokens attrs h Damage hero.damage
        when (hero.resources > 0) $ removeTokens attrs h Resource hero.resources
        guardCharacterWith h (EncounterCard watcher)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [() | Window _ (EnemyEngaged _ _) <- ws] \_ -> damageEachActiveLocation attrs 1
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      -- Stage 4A's when-revealed effect resolves before the advancement window.
      -- Doors of Durin is active when each Tentacle's engagement damages it.
      tentacles <- getSetAside (cardIs Enemies.paleGreenTentacle)
      players <- getRemainingPlayers
      for_ (zip players tentacles) $ \(pid, card) -> putEnemyIntoPlayEngaged pid card
      shuffleSetAsideIntoEncounterDeck (drop (length players) tentacles)
      shuffleEncounterDiscardIntoDeck AnyCard
      pure q
    CheckWindows ws
      | attrs.flipped
      , any
          (cardMatches (cardIs Locations.doorsOfDurinTheRingGoesSouth))
          [c | Window After (ExploredCard c) <- ws] -> do
          winGame
          TheGatesOfMoria <$> runMessage msg attrs
    _ -> TheGatesOfMoria <$> runMessage msg attrs
