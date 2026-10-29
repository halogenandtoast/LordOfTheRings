module LordOfTheRings.Quest.Cards.TheLastStage (theLastStage) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (addRavenCardToStaging)

newtype TheLastStage = TheLastStage QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLastStage :: QuestCard TheLastStage
theLastStage = quest TheLastStage Cards.theLastStage

-- | Forced: After the last time counter is removed from this stage, each engaged enemy
-- makes an attack. Place 2 time counters on this stage.
-- At the end of the round, if Raven Chief is in the victory display, the players win the
-- game.
instance HasAbilities TheLastStage where
  getAbilities (TheLastStage a) =
    [ ability
    | a.flipped
    , ability <-
        [ mkAbility a 1 $ forced $ lastTimeCounterRemoved a
        , restricted a 2 (CardInVictoryDisplay $ cardIs Enemies.ravenChief) $ forced RoundEndsWindow
        ]
    ]

-- | When Revealed: Add Raven Chief and Raven Chief's Camp to the staging area. Take the top
-- X cards from the Raven deck and add them to the staging area. X is the number of players
-- minus 1.
-- Each engaged enemy attacks the player it is engaged with, in turn order.
instance RunMessage TheLastStage where
  runMessage msg q@(TheLastStage attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheLastStage <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.ravenChief)
      addSetAsideToStaging (cardIs Locations.ravenChiefsCamp)
      players <- length <$> getRemainingPlayers
      replicateM_ (players - 1) addRavenCardToStaging
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> do
        engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
        for_ engaged \eid -> immediateAttack pid eid
      placeTimeCounters attrs attrs 2
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      winGame
      pure q
    _ -> TheLastStage <$> runMessage msg attrs
