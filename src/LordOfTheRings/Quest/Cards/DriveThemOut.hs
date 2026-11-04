module LordOfTheRings.Quest.Cards.DriveThemOut (driveThemOut) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Helpers.ControlledLocation (firstPlayerTakesControl, returnHighestThreatLocation)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype DriveThemOut = DriveThemOut QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

driveThemOut :: QuestCard DriveThemOut
driveThemOut = quest DriveThemOut Cards.driveThemOut

-- | Progress cannot be placed on locations in the staging area by player card effects.
-- The players cannot travel to Gate of Dorwinion unless there are more locations under the
-- players' control than in the staging area.
instance HasModifiersFor DriveThemOut where
  getModifiersFor (DriveThemOut a) = when a.flipped do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [NoProgressFromPlayerCards]
    controlled <- length <$> select (LocationControlledBy AnyPlayer)
    unless (controlled > length staged) do
      gates <- select (LocationWithCardCode $ toCardCode Locations.gateOfDorwinion)
      for_ gates \g -> modified a g [CannotBeTraveledTo]

-- | At the end of the round, if the players control Gate of Dorwinion, they win the game.
instance HasAbilities DriveThemOut where
  getAbilities (DriveThemOut a) =
    [ restricted a 1 (LocationExists $ LocationWithCardCode (toCardCode Locations.gateOfDorwinion) <> LocationControlledBy AnyPlayer)
        $ forced RoundEndsWindow
    | a.flipped
    ]

enemyCard :: CardMatcher
enemyCard = CardWithType EnemyType

{- | When Revealed: Each player returns the highest threat location he controls to the
staging area. Shuffle the encounter discard pile into the encounter deck. Starting with the
first player, each player discards cards from the top of the encounter deck until he
discards an enemy and puts that enemy into play engaged with him.
Forced: When a location is explored, the first player takes control of it.

Its 12 quest points are no stage to defeat, there being no stage 3: the progress on it lets
the players travel to Gate of Dorwinion once it reaches 12, and the game is won by
controlling Gate of Dorwinion at the end of a round.
-}
instance RunMessage DriveThemOut where
  runMessage msg q@(DriveThemOut attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DriveThemOut <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer (returnHighestThreatLocation attrs)
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep n inner@(WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (take 1 $ drop (n - 1) players) \pid -> do
        discardEncounterCardsUntilFor attrs (PlayerTarget pid) enemyCard
        doStep (n + 1) inner
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget pid) cards -> do
      for_ (find (cardMatches enemyCard) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged pid card
      pure q
    CheckWindows ws | attrs.flipped -> do
      for_ [lid | Window When (Explored lid) <- ws] firstPlayerTakesControl
      DriveThemOut <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      winGame
      pure q
    _ -> DriveThemOut <$> runConditionalStage (pure False) msg attrs
