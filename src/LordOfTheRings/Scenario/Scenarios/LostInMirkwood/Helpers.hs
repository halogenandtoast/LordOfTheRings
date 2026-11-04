-- | What the cards of Lost in Mirkwood share: its objective-locations, which advance the
-- players to the next stage's A or C version, and stage 4A's nemesis.
module LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (
  StageSide (..),
  isObjectiveLocation,
  objectiveLocationMatcher,
  objectiveLocationsInPlay,
  objectiveLocationAbilities,
  runObjectiveLocation,
  advanceToNextStage,
  nextStage,
  getNemesis,
  setNemesis,
  runEncounterSetStage,
  attackDamagedCharacter,
  advancingFromStage,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ability
import LordOfTheRings.Card (CardDef (cdEncounterSet, cdStage), EncounterCard, HasCardDef (toCardDef), toCardCode)
import LordOfTheRings.EncounterSet (EncounterSet)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Quest.Runner
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Trait (Trait)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id (CardId, EnemyId, LocationId)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Source (Source (EnemyAttackSource), isSource)
import LordOfTheRings.Target
import LordOfTheRings.Timing

-- | The version of a stage an objective-location advances to: A, or C.
data StageSide = SideA | SideC
  deriving stock (Show, Eq)

objectiveLocationCodes :: [CardCode]
objectiveLocationCodes =
  map toCardCode [Locations.overgrownPath, Locations.abandonedVillage, Locations.bareHilltop, Locations.forestClearingLostInMirkwood]

isObjectiveLocation :: HasCardDef a => a -> Bool
isObjectiveLocation = (`elem` objectiveLocationCodes) . toCardCode . toCardDef

-- | An objective-location's card.
objectiveLocationMatcher :: CardMatcher
objectiveLocationMatcher = CardMatchAny (map CardWithCode objectiveLocationCodes)

-- | The objective-locations in play: in the staging area, active, or guarded.
objectiveLocationsInPlay :: HasGame m => m [LocationId]
objectiveLocationsInPlay =
  getsGame (map (.id) . filter isObjectiveLocation . toList . (.entities.locations))

-- | Forced: After the players travel here, advance to the next stage A (or C), if able.
objectiveLocationAbilities :: LocationAttrs -> [Ability]
objectiveLocationAbilities a = [mkAbility a 101 $ forced $ TraveledWindow After a.id]

{- | Travel: Shuffle the encounter discard pile into the encounter deck. Each player
discards the top card of the encounter deck: the location reads the cards back as
'DiscardedTopOfEncounterDeck', with each player as the target. Its Forced effect
advances to the stage's side.
-}
runObjectiveLocation :: ReverseQueue m => StageSide -> Message -> LocationAttrs -> m ()
runObjectiveLocation side msg a = case msg of
  TravelTo lid | lid == a.id -> do
    shuffleEncounterDiscardIntoDeck AnyCard
    eachPlayer \p -> discardTopOfEncounterDeckFor a (PlayerTarget p) 1
  UseCardAbility _ (isSource a -> True) 101 _ -> advanceToNextStage side
  _ -> pure ()

-- | Advance to the next stage's version, if able: the scenario answers.
advanceToNextStage :: ReverseQueue m => StageSide -> m ()
advanceToNextStage side = push $ ScenarioSpecific "lostInMirkwood.advance" (toJSON $ side == SideA)

sideACodes :: [CardCode]
sideACodes = map toCardCode [Quests.spidersOfMirkwoodStage, Quests.carriedAway, Quests.theForestOfGreatFear]

{- | The stage of the quest deck the players advance to from the stage: one numbered 1
higher, of the side. "Next" is the stage numbered 1 higher, whichever version the players
are on, as the players read it (FFG forum, "Did I play Lost in Mirkwood right?" and "Lost
in 'Lost in Mirkwood'"; RingsDB's community notes say the same): from 2A an
objective-location saying C goes to 3C. No designer ruling exists: the FFG forum's
"Unofficial Official Rulings thread" has Caleb's Lost in Mirkwood ruling only on Guarded
(location).
-}
nextStage :: Bool -> Int -> [EncounterCard] -> Maybe EncounterCard
nextStage sideA current =
  find \c -> stageOf c == current + 1 && (toCardCode c `elem` sideACodes) == sideA
 where
  stageOf = fromMaybe 0 . cdStage . toCardDef

-- | The nemesis of stage 4A, until it leaves play: the enemy in play with the card the
-- scenario recorded.
getNemesis :: HasGame m => m (Maybe EnemyId)
getNemesis = do
  meta <- getsGame ((.meta) . toAttrs . (.scenario))
  case parseMaybe parseJSON meta of
    Just (cid :: CardId) -> getsGame (fmap (.id) . find ((== cid) . (.cardId) . toAttrs) . toList . (.entities.enemies))
    _ -> pure Nothing

-- | The card is the nemesis once in play; Nothing as the nemesis leaves play.
setNemesis :: ReverseQueue m => Maybe CardId -> m ()
setNemesis = push . ScenarioSpecific "lostInMirkwood.nemesis" . toJSON

{- | Stages 2A and 2C: "When Revealed: Shuffle the (set) encounter set into the encounter
deck. Then, reveal 1 encounter card per player." The lead clicks the stage to resolve it.
-}
runEncounterSetStage :: EncounterSet -> Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runEncounterSetStage set msg attrs = case msg of
  Flip (isTarget attrs -> True) | not attrs.flipped -> do
    askToResolveWhenRevealed attrs
    runMessage msg attrs
  WhenRevealed (isTarget attrs -> True) -> do
    aside <- getsGame (filter ((== Just set) . cdEncounterSet . toCardDef) . scenarioSetAside . toAttrs . (.scenario))
    shuffleSetAsideIntoEncounterDeck aside
    players <- getRemainingPlayers
    revealEncounterCards (length players)
    pure attrs
  _ -> runMessage msg attrs

{- | The character an attack by an enemy of the trait damaged, read as its damage is dealt,
as Giant Spider's "attacks and damages a character".
-}
attackDamagedCharacter :: HasGame m => Trait -> Message -> m (Maybe Target)
attackDamagedCharacter trait = \case
  DoStep 1 (DealDamage (EnemyAttackSource eid) target n) | n > 0, isCharacter target -> do
    matches <- selectAny (EnemyWithId eid <> EnemyWithTrait trait)
    pure $ guard matches $> target
  _ -> pure Nothing
 where
  isCharacter = \case
    HeroTarget _ -> True
    AllyTarget _ -> True
    _ -> False

-- | The players advance from the stage in play, as "when the players advance from this
-- stage" reads: the scenario says so before the stage leaves play.
advancingFromStage :: Message -> Bool
advancingFromStage = \case
  ScenarioSpecific "lostInMirkwood.advancing" _ -> True
  _ -> False
