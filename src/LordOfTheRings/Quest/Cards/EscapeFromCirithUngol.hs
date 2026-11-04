module LordOfTheRings.Quest.Cards.EscapeFromCirithUngol (escapeFromCirithUngol) where

import LordOfTheRings.Card (Card (EncounterCard), EncounterCard (..), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing)
import LordOfTheRings.Trait (Trait (RingBearer))

newtype EscapeFromCirithUngol = EscapeFromCirithUngol QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeFromCirithUngol :: QuestCard EscapeFromCirithUngol
escapeFromCirithUngol = quest EscapeFromCirithUngol Cards.escapeFromCirithUngol

-- | The players cannot travel to The Two Watchers unless there is at least 12 progress on
-- this stage. While The Two Watchers is in the staging area (enemy side faceup), it gets +3
-- threat and +3 defense and is considered to be engaged with each player.
instance HasModifiersFor EscapeFromCirithUngol where
  getModifiersFor (EscapeFromCirithUngol a) = when a.flipped do
    when (a.progress < 12) do
      locations <- select (LocationWithCardCode $ toCardCode Locations.theTwoWatchers)
      for_ locations \l -> modified a l [CannotBeTraveledTo]
    watchers <- select (EnemyWithCardCode (toCardCode Enemies.theTwoWatchersEnemy) <> EnemyInStagingArea)
    for_ watchers \e -> modified a e [ThreatModifier 3, DefenseModifier 3, EngagedWithEachPlayer AnyPlayer]

-- | Once The Two Watchers' enemy side is defeated, the stage checks again whether it is.
instance HasAbilities EscapeFromCirithUngol where
  getAbilities (EscapeFromCirithUngol a) = [mkAbility a 1 $ silent $ EnemyDestroyedWindow After (cardIs Enemies.theTwoWatchersEnemy) | a.flipped]

-- | Neither side of The Two Watchers is in play.
watchersGone :: QueueT Message GameT Bool
watchersGone = do
  enemy <- selectAny (EnemyWithCardCode $ toCardCode Enemies.theTwoWatchersEnemy)
  location <- selectAny (LocationWithCardCode $ toCardCode Locations.theTwoWatchers)
  pure $ not (enemy || location)

{- | When Revealed: Add Last of the Company to the victory display. Attach The One Ring to
the Ring-bearer and exhaust the Ring-bearer. The first player moves The Two Watchers from
the victory display to the staging area (location side faceup). Each other player reveals
an encounter card.

This stage cannot be defeated while The Two Watchers is in play. If this stage is
defeated, the players win the game.
-}
instance RunMessage EscapeFromCirithUngol where
  runMessage msg q@(EscapeFromCirithUngol attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      EscapeFromCirithUngol <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      company <- getsGame (filter ((== toCardCode Objectives.lastOfTheCompany) . toCardCode) . toList . (.entities.objectives))
      for_ company (addToVictoryDisplay . ObjectiveTarget . toId)
      doStep 1 msg
      pure q
    -- the Ring-bearer, once Last of the Company has left
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      ringBearers <- select (HeroWithTrait RingBearer)
      ring <- getSagaOneRing
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "escapeFromCirithUngol.ringBearer"
        causedBy attrs
        targets ringBearers \hid -> do
          for_ ring \r -> placeObjective (toId r) (AttachedTo $ HeroTarget hid)
          exhaust (HeroTarget hid)
      display <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      for_ [c | EncounterCard c <- display, toCardCode c `elem` map toCardCode [Enemies.theTwoWatchersEnemy, Locations.theTwoWatchers]] \c -> do
        removeFromVictoryDisplay (EncounterCard c)
        addToStaging c {ecCardCode = toCardCode Locations.theTwoWatchers}
      others <- filter (/= lead) <$> getRemainingPlayers
      for_ others revealEncounterCardAs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      push $ CheckStage attrs.id
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      winGame
      pure q
    _ -> EscapeFromCirithUngol <$> runConditionalStage watchersGone msg attrs
