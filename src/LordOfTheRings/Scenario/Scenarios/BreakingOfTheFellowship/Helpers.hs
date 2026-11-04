{- | What Breaking of the Fellowship's cards share.

Stage 2B has each player create his own staging area, with his own stage 3 (The Road
Darkens rules insert, "Multiple Staging Areas"): unlike earlier separate stages, the
players still interact through the normal rules of the game, so the areas share it (see
'LordOfTheRings.Game.Base.Separation'). "At this stage" and "this staging area" are the
area in focus's, "any staging area" each area's.
-}
module LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (
  playersAtThisStage,
  revealEncounterCardAt,
  stagingAreaToTheLeft,
  enemiesInAnyStagingArea,
  nonUniqueLocationsInAnyStagingArea,
  toFirstPlayersStagingArea,
  printedHitPoints,
  attacksTheWeaker,
  defendersWeakerThan,
  isBurden,
  frodosChoiceInPlay,
  isFrodosChoice,
  frodosChoiceRevealed,
  stageThreeModifiers,
) where

import LordOfTheRings.Card (CardDef (..), CardType (..), HasCardCode, toCardCode, toCardDef)
import LordOfTheRings.Trait (Trait (Boon))
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Entities (Entities)
import LordOfTheRings.Game.Base (Area (..), Separation (..), gameEntities, gamePlayerOrder, gameRevealer, gameSeparation, heldEntities, seatsFrom)
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message (SetRevealer))
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.Modifier (ModifierType (AttackModifier, QuestProgressCanceledBy))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.Types (QuestAttrs)
import LordOfTheRings.Window (Window (..), WindowType (Revealed))
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

-- | The players still in the game at the stage in focus, in turn order.
playersAtThisStage :: HasGame m => m [PlayerId]
playersAtThisStage = filterM (fmap (not . (.eliminated)) . getPlayer) =<< getStagePlayers

{- | The player reveals an encounter card at his own stage, its "you": as Uruk-hai Hunter
has a chosen player reveal one. The stage in focus is in focus again after.
-}
revealEncounterCardAt :: ReverseQueue m => PlayerId -> m ()
revealEncounterCardAt pid = do
  here <- getStageLead
  revealer <- getsGame gameRevealer
  focusAreaOf pid
  push $ SetRevealer (Just pid)
  revealEncounterCard
  push $ SetRevealer revealer
  focusAreaOf here

{- | The first player of the staging area to the left of the stage in focus: the next area
in turn order, the players passing to the left. None while the players share one.
-}
stagingAreaToTheLeft :: HasGame m => m (Maybe PlayerId)
stagingAreaToTheLeft = do
  here <- getStagePlayers
  sep <- getsGame gameSeparation
  order <- getsGame gamePlayerOrder
  pure $ case (sep, here) of
    (Just s, p : _) | separationShared s -> do
      let areaOf q = find (elem q . areaPlayers) (separationAreas s)
      next <- find (\q -> q `notElem` here && isJust (areaOf q)) (drop 1 $ seatsFrom p order)
      pure next
    _ -> Nothing

-- | The cards of each area, the one in focus first.
stagingCards :: HasGame m => m [Entities]
stagingCards = getsGame \g -> gameEntities g : heldEntities g

-- | The enemies in any staging area.
enemiesInAnyStagingArea :: HasGame m => m [EnemyId]
enemiesInAnyStagingArea = do
  areas <- stagingCards
  pure [toId e | es <- areas, e <- toList es.enemies, e.placement == StagingArea]

-- | The non-unique locations in any staging area.
nonUniqueLocationsInAnyStagingArea :: HasGame m => m [LocationId]
nonUniqueLocationsInAnyStagingArea = do
  areas <- stagingCards
  pure [toId l | es <- areas, l <- toList es.locations, l.placement == StagingArea, not (cdUnique $ toCardDef l)]

-- | The card goes to the first player's staging area.
toFirstPlayersStagingArea :: (ReverseQueue m, Targetable target) => target -> m ()
toFirstPlayersStagingArea target = do
  lead <- getLead
  placeInStagingAreaOf lead target

-- | The character's printed hit points.
printedHitPoints :: HasGame m => Target -> m Int
printedHitPoints t = fromMaybe 0 . cdHitPoints <$> characterCardDef t

-- | The defending characters with fewer printed hit points than the attacking enemy's.
defendersWeakerThan :: HasGame m => EnemyId -> [Target] -> m [Target]
defendersWeakerThan eid defenders = do
  e <- getEnemy eid
  let printed = fromMaybe 0 (cdHitPoints $ toCardDef e)
  filterM (fmap (< printed) . printedHitPoints) defenders

{- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending character has
fewer printed hit points than the attacking enemy). Undefended, there is no defending
character, and it gets +1.
-}
attacksTheWeaker :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> [Target] -> m ()
attacksTheWeaker source eid defenders = do
  weaker <- defendersWeakerThan eid defenders
  attackModifier source eid (AttackModifier $ if null weaker then 1 else 2)

-- | A burden: an encounter card of a saga's burden set, its set icon in place of an
-- encounter set, as Seat of Seeing reads.
isBurden :: CardDef -> Bool
isBurden def =
  isJust (cdSetIcon def)
    && cdCardType def `elem` [TreacheryType, ObjectiveType, EnemyType, LocationType]
    && not (Boon `member` cdCardTraits def)

-- | The card is Frodo's Choice.
isFrodosChoice :: HasCardCode a => a -> Bool
isFrodosChoice = (== toCardCode Objectives.frodosChoice) . toCardCode

-- | Frodo's Choice is in play, attached to the Ring-bearer.
frodosChoiceInPlay :: HasGame m => m Bool
frodosChoiceInPlay = getsGame (any isFrodosChoice . (.entities.objectives))

-- | The windows are those after Frodo's Choice was revealed.
frodosChoiceRevealed :: HasGame m => [Window] -> m Bool
frodosChoiceRevealed ws = do
  objectives <- getsGame (.entities.objectives)
  pure $ or [isFrodosChoice o | Window After (Revealed (ObjectiveTarget oid)) <- ws, Just o <- [lookup oid objectives]]

{- | Each stage 3B: "If Frodo's Choice is not in play, any time players would place progress
on this quest, discard an equal number of cards from the encounter deck instead." The
progress questing would place is canceled, and the stage told how much ('ProgressCanceled').
-}
stageThreeModifiers :: HasModifiersM m => QuestAttrs -> m ()
stageThreeModifiers a = when a.flipped do
  inPlay <- frodosChoiceInPlay
  unless inPlay $ modified a GameTarget [QuestProgressCanceledBy (toSource a)]
