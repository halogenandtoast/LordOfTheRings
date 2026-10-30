module LordOfTheRings.Quest.Cards.DefendTheShip (defendTheShip) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, cdCardType, cdKeywords, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gamePhase, gameScenario)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Keyword (isGrapple)
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Trait (Trait (Ship, Tentacle))

newtype DefendTheShip = DefendTheShip QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

defendTheShip :: QuestCard DefendTheShip
defendTheShip = quest DefendTheShip Cards.defendTheShip

-- | During the travel phase, the players may travel to a location in the staging area even
-- if another location is currently active. If they do, return the currently active
-- location to the staging area.
instance HasModifiersFor DefendTheShip where
  getModifiersFor (DefendTheShip a) = when a.flipped do
    phase <- getsGame gamePhase
    when (phase == TravelPhase) do
      staged <- select LocationInStagingArea
      for_ staged \l -> modified a l [TravelableWithActiveLocation]

-- | When Revealed: Discard cards from the top of the encounter deck until X Tentacle
-- enemies are discarded, where X is 1 more than the number of players in the game. Resolve
-- the Grapple keyword on each of those enemies.
-- ponytail: as many are discarded as the deck holds before the Xth Tentacle enemy; its
-- discard pile was just shuffled in, so it holds them all
-- At the end of the round, if the total threat of any one Ship location is 9 or higher,
-- the Stormcaller is destroyed, and the players lose the game.
-- When The Thing in the Depths is destroyed, the players win the game.
instance RunMessage DefendTheShip where
  runMessage msg q@(DefendTheShip attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DefendTheShip <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      deck <- getsGame ((.scenarioEncounterDeck) . toAttrs . gameScenario)
      let k = maybe (length deck) (+ 1) $ headMay $ drop players [i | (i, c) <- zip [0 ..] deck, tentacle c]
      discardTopOfEncounterDeck attrs k
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (\c -> tentacle c && any isGrapple (cdKeywords $ toCardDef c)) cards) grappleCard
      pure q
    TravelTo lid | attrs.flipped -> do
      active <- select (IsActiveLocation <> not_ (LocationWithId lid))
      for_ active \l -> placeLocation l StagingArea
      DefendTheShip <$> runMessage msg attrs
    CheckWindows ws | attrs.flipped -> do
      when (any ((== RoundEnds) . windowType) ws) do
        threats <- traverse getLocationThreat =<< select (LocationWithTrait Ship)
        when (any (>= 9) threats) $ gameOver Lost
      for_ [eid | Window When (EnemyDefeated eid) <- ws] \eid ->
        whenM (selectAny $ EnemyWithId eid <> EnemyWithCardCode (toCardCode Enemies.theThingInTheDepths)) winGame
      DefendTheShip <$> runMessage msg attrs
    _ -> DefendTheShip <$> runMessage msg attrs
   where
    tentacle c = cdCardType (toCardDef c) == EnemyType && Tentacle `member` cdCardTraits (toCardDef c)
