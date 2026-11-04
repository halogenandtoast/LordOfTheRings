module LordOfTheRings.Quest.Cards.AssaultOnDolRhugar (assaultOnDolRhugar) where

import LordOfTheRings.Card (cdCardType, isSideQuestType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (castlesInVictoryDisplay)

newtype AssaultOnDolRhugar = AssaultOnDolRhugar QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

assaultOnDolRhugar :: QuestCard AssaultOnDolRhugar
assaultOnDolRhugar = quest AssaultOnDolRhugar Cards.assaultOnDolRhugar

{- | Thane Ulchor cannot leave the staging area.
While there are at least 3 progress tokens on this stage, it cannot be chosen as the
current quest stage during the quest phase.
-}
instance HasModifiersFor AssaultOnDolRhugar where
  getModifiersFor (AssaultOnDolRhugar a) = when a.flipped do
    thanes <- select thaneUlchor
    for_ thanes \t -> modified a t [CannotLeaveStagingArea, CannotBeOptionallyEngagedBy AnyPlayer]
    when (a.progress >= 3) $ modifySelf a [CannotBeCurrentQuest]

-- | This stage cannot be defeated unless it is the end of the round and there are 3 Castle
-- side quests in the victory display.
instance HasAbilities AssaultOnDolRhugar where
  getAbilities (AssaultOnDolRhugar a) = [mkAbility a 1 $ silent RoundEndsWindow | a.flipped]

{- | Setup: Player decks cannot exceed 50 cards (the game refuses a larger deck). Create the
Power of Mordor deck and set it next to the quest deck. Each player puts 1 copy of Ulchor's
Guard into play engaged with him. Add Thane Ulchor and all 4 copies of the Storm the Castle
side quest to the staging area, randomized so that the players do not know which side
quest is on the other side of each (rule sheet). Place the top card of the Power of Mordor
deck faceup under each side quest in play. Each player places 8 cards from top of his deck
under the quest deck and each side quest in play.
-}
instance RunMessage AssaultOnDolRhugar where
  runMessage msg q@(AssaultOnDolRhugar attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      createPowerOfMordorDeck
      players <- getRemainingPlayers
      guards <- getsGame (filter (cardMatches $ cardIs Enemies.ulchorsGuard) . (.encounterDeck) . toAttrs . (.scenario))
      for_ (zip players guards) \(p, ulchorsGuard) -> do
        removeFromEncounterDeck ulchorsGuard
        putEnemyIntoPlayEngaged p ulchorsGuard
      addSetAsideToStaging (cardIs Enemies.thaneUlchorTheFortressOfNurn)
      storms <- shuffleM =<< getSetAside stormTheCastle
      traverse_ addToStaging storms
      doStep 1 msg
      AssaultOnDolRhugar <$> runMessage msg attrs
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      sideQuests <- getsGame (map toTarget . filter (isSideQuestType . cdCardType . toCardDef) . toList . entitiesQuests . gameEntities)
      for_ sideQuests placeTopOfPowerOfMordorDeckUnderneath
      eachPlayer \p -> do
        deck <- (.deck) <$> getPlayer p
        for_ (zip (ScenarioTarget : sideQuests) (eights deck)) \(under, cards) ->
          placeUnderneath under cards
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n -> pure $ AssaultOnDolRhugar $ attrs & tokensL %~ addTokens Progress n
    CheckStage qid | qid == attrs.id -> pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      castles <- castlesInVictoryDisplay
      done <- hasEnoughProgress attrs
      when (castles >= 3 && done) $ completeQuest attrs.id
      pure q
    _ -> AssaultOnDolRhugar <$> runMessage msg attrs
   where
    eights = \case
      [] -> []
      cards -> take 8 cards : eights (drop 8 cards)
    stormTheCastle =
      CardMatchAny
        $ map cardIs [Cards.stormTheCastleCastleGarrison, Cards.stormTheCastleLethalCounterattack, Cards.stormTheCastleNoQuarter, Cards.stormTheCastleStrongholdOfMordor]
