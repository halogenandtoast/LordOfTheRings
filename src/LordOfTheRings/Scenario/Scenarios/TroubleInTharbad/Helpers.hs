-- | What the cards of Trouble in Tharbad share: Nalir, the attacks made against him, the
-- threat elimination level Double Dealings lowers, and The Empty Mug in the victory display.
module LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (
  getNalir,
  attackNalir,
  anyThreatAtMost20,
  lowerEliminationLevels,
  lowerEliminationLevelsBy,
  eliminationLowering,
  eliminationLevelsLowered,
  emptyMugAbility,
  removeCardThatReducedThreat,
  canRemoveQuestTimeCounter,
  attackingEnemyGetsBonus,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ability
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Id (AllyId, EnemyId, PlayerId)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (CannotRemoveTimeCounters, EliminationThreatModifier, UndefendedDamageTo))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioMeta)
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Window

-- | Nalir, under a player's control.
getNalir :: HasGame m => m (Maybe AllyId)
getNalir = headMay <$> select (AllyWithCardCode $ toCardCode Objectives.nalirTroubleInTharbad)

{- | The enemy makes an immediate attack against Nalir: an attack against the player who
controls him, its undefended damage assigned to Nalir (the rules sheet). With Nalir gone
there is no attack.
-}
attackNalir :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> m ()
attackNalir source eid =
  getNalir >>= traverse_ \n -> do
    controller <- placementController . (.placement) <$> getAlly n
    for_ controller \pid -> do
      attackModifier source eid (UndefendedDamageTo $ toTarget n)
      immediateAttack pid eid

-- | "If any player's threat is 20 or less."
anyThreatAtMost20 :: HasGame m => m Bool
anyThreatAtMost20 = anyM (fmap ((<= 20) . (.threat)) . getPlayer) =<< getRemainingPlayers

-- | Double Dealings lowers each player's threat elimination level by 10 more, for the
-- rest of the game.
lowerEliminationLevels :: ReverseQueue m => m ()
lowerEliminationLevels = lowerEliminationLevelsBy 10

-- | Lower each player's threat elimination level by this much more, for the rest of the
-- game, as the nightmare cards do by 2 or 5.
lowerEliminationLevelsBy :: ReverseQueue m => Int -> m ()
lowerEliminationLevelsBy = push . ScenarioSpecific "tharbad.lowerElimination" . toJSON

{- | How much the scenario's meta has each player's elimination level lowered by: an object
of the amount, or, as a game saved before the nightmare cards keeps it, the times Double
Dealings lowered it by 10.
-}
eliminationLowering :: Value -> Int
eliminationLowering v =
  fromMaybe 0 $ parseMaybe (withObject "lowered" (.: "lowered")) v <|> ((* 10) <$> parseMaybe parseJSON v)

-- | How much each elimination level has been lowered, kept by the scenario.
loweredBy :: HasGame m => m Int
loweredBy = getsGame (eliminationLowering . scenarioMeta . toAttrs . gameScenario)

{- | The elimination level each player has, as either stage gives it: the scenario keeps the
count, the stage in play applies it.
-}
eliminationLevelsLowered :: (HasModifiersM m, Sourceable source) => source -> m ()
eliminationLevelsLowered source = do
  n <- loweredBy
  players <- getRemainingPlayers
  when (n > 0) $ for_ players \p -> modified source (PlayerTarget p) [EliminationThreatModifier (negate n)]

{- | While The Empty Mug is in the victory display, the current quest gains: "Forced: After
a player card effect reduces a player's threat, remove it from the game."
-}
emptyMugAbility :: Sourceable a => a -> Int -> Ability
emptyMugAbility a idx =
  restricted a idx (CardInVictoryDisplay $ cardIs Locations.theEmptyMug)
    $ forced
    $ ThreatReducedByPlayerCardWindow After

{- | Whether a time counter can be removed from the current quest, as "the first player
either removes 1 time counter from the current quest or..." offers: one is there, and
nothing, as Tharbad Hideout, stops it.
-}
canRemoveQuestTimeCounter :: HasGame m => m Bool
canRemoveQuestTimeCounter =
  getCurrentQuest >>= \case
    Nothing -> pure False
    Just q -> (&&) . (> 0) <$> getTimeCounters q <*> (notElem CannotRemoveTimeCounters <$> getModifiers q)

-- | "Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending player's
-- threat is 20 or less)."
attackingEnemyGetsBonus :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> PlayerId -> m ()
attackingEnemyGetsBonus source eid pid = do
  threat <- (.threat) <$> getPlayer pid
  enemyGetsAttack source eid (if threat <= 20 then 2 else 1)

-- | The Empty Mug's Forced, as the stage resolves it.
removeCardThatReducedThreat :: ReverseQueue m => [Window] -> m ()
removeCardThatReducedThreat ws =
  for_ (take 1 [s | Window _ (ThreatReduced _ s) <- ws, isPlayerCardSource s]) removeSourceCardFromGame
