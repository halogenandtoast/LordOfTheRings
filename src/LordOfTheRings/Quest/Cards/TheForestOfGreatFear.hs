module LordOfTheRings.Quest.Cards.TheForestOfGreatFear (theForestOfGreatFear) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, cdHitPoints, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (getNemesis, setNemesis)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))

newtype TheForestOfGreatFear = TheForestOfGreatFear QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Stage 4A of Lost in Mirkwood.
theForestOfGreatFear :: QuestCard TheForestOfGreatFear
theForestOfGreatFear = quest TheForestOfGreatFear Cards.theForestOfGreatFear

{- | The nemesis gets +2 threat, +2 attack, +2 defense and is immune to player card effects.
This stage cannot be defeated while the nemesis is in play. The nemesis cannot take damage
unless there is at least 8 progress on this stage.
-}
instance HasModifiersFor TheForestOfGreatFear where
  getModifiersFor (TheForestOfGreatFear a) = when a.flipped do
    getNemesis >>= traverse_ \eid -> do
      modified a eid $ [ThreatModifier 2, AttackModifier 2, DefenseModifier 2, ImmuneToPlayerCardEffects] <> [CannotTakeDamage | a.progress < 8]
      modifySelf a [CannotBeDefeated]

-- | Forced: At the end of the round, return the nemesis to the staging area and it makes an
-- attack against the first player.
instance HasAbilities TheForestOfGreatFear where
  getAbilities (TheForestOfGreatFear a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | When Revealed: Remove Searching for a Way Out from the game. Search the remaining
set-aside encounter set for the enemy with the most hit points and add it to the staging
area. That enemy is the "nemesis" until it leaves play: Ungoliant's Spawn of Spiders of
Mirkwood, or Chieftan Ufthak of Dol Guldur Orcs.
If the players defeat the nemesis, they win the game.
-}
instance RunMessage TheForestOfGreatFear where
  runMessage msg q@(TheForestOfGreatFear attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheForestOfGreatFear <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      searching <- getsGame (filter ((== toCardCode Objectives.searchingForAWayOut) . toCardCode) . toList . (.entities.objectives))
      for_ searching removeFromGame
      aside <- getsGame (filter ((== EnemyType) . cdCardType . toCardDef) . scenarioSetAside . toAttrs . (.scenario))
      for_ (maximumByMay (comparing (cdHitPoints . toCardDef)) aside) \nemesis -> do
        addToStaging nemesis
        setNemesis (Just nemesis.id)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      getNemesis >>= traverse_ \eid -> do
        lead <- getLead
        placeEnemy eid StagingArea
        immediateAttack lead eid
      pure q
    Destroy _ (EnemyTarget eid) -> do
      nemesis <- getNemesis
      when (nemesis == Just eid) winGame
      TheForestOfGreatFear <$> runMessage msg attrs
    _ -> do
      leaving <- case msg of
        Discard (EnemyTarget eid) -> pure (Just eid)
        RemoveFromGame (EnemyTarget eid) -> pure (Just eid)
        ShuffleIntoEncounterDeck (EnemyTarget eid) -> pure (Just eid)
        ReturnToTopOfEncounterDeck (EnemyTarget eid) -> pure (Just eid)
        _ -> pure Nothing
      nemesis <- getNemesis
      when (isJust leaving && leaving == nemesis) $ setNemesis Nothing
      TheForestOfGreatFear <$> runMessage msg attrs
