module LordOfTheRings.Quest.Cards.StopTheWarParty (stopTheWarParty) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype StopTheWarParty = StopTheWarParty QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

stopTheWarParty :: QuestCard StopTheWarParty
stopTheWarParty = quest StopTheWarParty Cards.stopTheWarParty

-- | Enemies do not make engagement checks.
instance HasModifiersFor StopTheWarParty where
  getModifiersFor (StopTheWarParty a) = when a.flipped do
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [NoEngagementChecks]

-- | Forced: At the end of the refresh phase, raise each player's threat by 1 for each enemy
-- in the staging area.
instance HasAbilities StopTheWarParty where
  getAbilities (StopTheWarParty a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase | a.flipped]

{- | Setup: Put Iârion into play. Add 1 Orc War Party to the staging area. Each player adds
1 different location to the staging area. Shuffle the encounter deck.
If the players control Iârion and defeat this stage, they win the game.

The players cannot win while an Orc War Party is in play, as its text reads: the stage's
progress piles up until no Orc War Party is left and the players control Iârion. Each
player searches in turn, from the first player, for a location unlike those already in the
staging area.
-}
instance RunMessage StopTheWarParty where
  runMessage msg q@(StopTheWarParty attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let copies def = filter (cardMatches $ cardIs def) deck
      -- Iârion joins the first player as he comes out
      for_ (take 1 (copies Objectives.iarion) <> take 1 (copies Enemies.orcWarParty)) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      doStep 1 msg
      pure $ StopTheWarParty $ attrs {questFlipped = True}
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
      n <- length <$> select EnemyInStagingArea
      eachPlayer \p -> encounterRaisesThreat p n
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      winGame
      pure q
    -- what may let the players win now: an Orc War Party gone, or Iârion back
    Discard _ (EnemyTarget _) -> recheck
    TakeControl {} -> recheck
    _ -> StopTheWarParty <$> runConditionalStage canWin msg attrs
   where
    recheck = do
      checkStage attrs.id
      StopTheWarParty <$> runConditionalStage canWin msg attrs
    canWin = do
      warParty <- selectAny (EnemyWithCardCode $ toCardCode Enemies.orcWarParty)
      iarion <- selectAny (AllyWithCardCode (toCardCode Objectives.iarion) <> AllyControlledBy AnyPlayer)
      pure $ not warParty && iarion
