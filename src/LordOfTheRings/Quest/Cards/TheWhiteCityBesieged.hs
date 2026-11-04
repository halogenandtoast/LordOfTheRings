module LordOfTheRings.Quest.Cards.TheWhiteCityBesieged (theWhiteCityBesieged) where

import Data.Traversable (mapAccumL)
import LordOfTheRings.Card (CardType (EnemyType), EncounterCard, cdName, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getLocationQuestPoints)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (InPlayArea, StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (getMinasTirith)
import LordOfTheRings.Trait (Trait (Pelennor))

newtype TheWhiteCityBesieged = TheWhiteCityBesieged QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theWhiteCityBesieged :: QuestCard TheWhiteCityBesieged
theWhiteCityBesieged = quest TheWhiteCityBesieged Cards.theWhiteCityBesieged

{- | Assault: progress cannot be placed on this stage by player card effects or by questing
successfully, and the players quest unsuccessfully by dealing the difference to Minas
Tirith, once for the group, instead of raising each player's threat (MEC54 rules,
"Assault"). Its own Forced effect places its progress (see 'placeOwnProgress').
The White City Besieged gets +4 quest points for each player. The players cannot travel to
Pelennor locations.
-}
instance HasModifiersFor TheWhiteCityBesieged where
  getModifiersFor (TheWhiteCityBesieged a) = when a.flipped do
    players <- length <$> getRemainingPlayers
    modifySelf a [QuestPointModifier (4 * players), ProgressLimit 0]
    getMinasTirith >>= traverse_ \o -> modified a GameTarget [QuestFailureDamagesInstead (ObjectiveTarget o)]
    pelennor <- select (LocationWithTrait Pelennor)
    for_ pelennor \l -> modified a l [CannotBeTraveledTo]

-- | Forced: When an enemy is destroyed, place progress on this stage equal to its total
-- threat (progress must be placed on any active location first).
instance HasAbilities TheWhiteCityBesieged where
  getAbilities (TheWhiteCityBesieged a) = [mkAbility a 1 $ forced $ EnemyDefeatedWindow When | a.flipped]

{- | When Revealed: Each player takes control of each ally he owns in the staging area and
exhausts it. The first player adds Minas Tirith, Grond, and The Witch-king (Captain side
faceup) to the staging area. Each other player searches the encounter deck and discard
pile for a different enemy and adds it to the staging area: one after another, each
passing over the titles the players before him took.
-}
instance RunMessage TheWhiteCityBesieged where
  runMessage msg q@(TheWhiteCityBesieged attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheWhiteCityBesieged <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- getsGame (filter ((== StagingArea) . (.placement)) . toList . (.entities.allies))
      for_ allies \ally -> do
        placeAlly ally.id (InPlayArea ally.owner)
        exhaust ally.id
      addSetAsideToStaging $ CardMatchAny $ map (CardWithCode . toCardCode) [Objectives.minasTirith, Locations.grond, Enemies.theWitchKing]
      doStep 2 (Run [msg])
      pure q
    DoStep n (Run (WhenRevealed (isTarget attrs -> True) : found)) -> do
      players <- getRemainingPlayers
      for_ (atMay players (n - 1)) \p -> do
        scenario <- getsGame (toAttrs . (.scenario))
        let taken = [title c | AddToStaging c <- found]
            enemy c = cdCardType' c && title c `notElem` taken
            fromDeck = filter enemy scenario.encounterDeck
            fromDiscard = filter enemy scenario.encounterDiscard
            next c = doStep (n + 1) (Run (WhenRevealed (toTarget attrs) : AddToStaging c : found))
        chooseOrRunOneM p do
          questionLabeled "theWhiteCityBesieged.search"
          causedBy attrs
          for_ fromDeck \c -> targeting (CardIdTarget c.id) do
            removeFromEncounterDeck c
            addToStaging c
            shuffleEncounterDeck
            next c
          for_ fromDiscard \c -> targeting (CardIdTarget c.id) do
            removeFromEncounterDiscard c
            addToStaging c
            shuffleEncounterDeck
            next c
          when (null fromDeck && null fromDiscard) $ labeled "continue" do
            shuffleEncounterDeck
            doStep (n + 1) (Run (WhenRevealed (toTarget attrs) : found))
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [eid | Window When (EnemyDefeated eid) <- ws] \eid -> do
        threat <- getEnemyThreat eid
        placeOwnProgress threat
      pure q
    -- the progress past the active location goes on the stage, though Assault keeps all
    -- other progress off it
    DoStep 1 (PlaceTokens source (isTarget attrs -> True) Progress n) ->
      TheWhiteCityBesieged <$> runMessage (PlaceTokens source (toTarget attrs) Progress n) attrs
    _ -> TheWhiteCityBesieged <$> runMessage msg attrs
   where
    title :: EncounterCard -> Text
    title c = let Name t _ = cdName (toCardDef c) in t
    cdCardType' c = cardMatches (CardWithType EnemyType) c
    placeOwnProgress n = when (n > 0) do
      actives <- select IsActiveLocation
      rooms <- for actives \lid -> do
        l <- getLocation lid
        (lid,) . subtract l.progress <$> getLocationQuestPoints lid
      let (rest, placed) = mapAccumL (\m (lid, room) -> let k = min m (max 0 room) in (m - k, (lid, k))) n rooms
      for_ placed \(lid, k) -> when (k > 0) $ placeProgress attrs lid k
      when (rest > 0) $ doStep 1 (PlaceTokens (toSource attrs) (toTarget attrs) Progress rest)
