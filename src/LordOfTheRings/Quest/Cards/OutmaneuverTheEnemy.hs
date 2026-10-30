module LordOfTheRings.Quest.Cards.OutmaneuverTheEnemy (outmaneuverTheEnemy) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (dolAmrothObjectivesToSearch, objectiveWithCardId, raiderFlagship)

newtype OutmaneuverTheEnemy = OutmaneuverTheEnemy QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

outmaneuverTheEnemy :: QuestCard OutmaneuverTheEnemy
outmaneuverTheEnemy = quest OutmaneuverTheEnemy Cards.outmaneuverTheEnemy

-- | If your current heading is at the worst setting, Raider Flagship gets -50 engagement
-- cost.
instance HasModifiersFor OutmaneuverTheEnemy where
  getModifiersFor (OutmaneuverTheEnemy a) = when a.flipped do
    whenM atWorstHeading do
      flagships <- select raiderFlagship
      for_ flagships \f -> modified a f [EngagementCostModifier (-50)]

{- | Setup: Prepare the Corsair deck. Players prepare their fleet. Set each copy of Dol
Amroth Warship aside, out of play. Search the encounter deck for Raider Flagship and 1
copy of Scouting Ship, and add them to the staging area. Search the encounter deck for a
Dol Amroth objective of your choice and add it to the staging area. Then, shuffle the
encounter deck and resolve the Guarded keyword on that objective. The scenario has set the
Dol Amroth Warships aside.

Sailing. Forced: When a Ship enemy engages you, if you are off-course, deal 2 damage to a
Ship objective you control. If you are on-course, deal 2 damage to that Ship enemy instead.
-}
instance RunMessage OutmaneuverTheEnemy where
  runMessage msg q@(OutmaneuverTheEnemy attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      prepareCorsairDeck
      prepareFleet
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ [Enemies.raiderFlagship, Enemies.scoutingShip] \def ->
        for_ (take 1 $ filter (cardMatches $ cardIs def) deck) \card -> do
          removeFromEncounterDeck card
          addToStaging card
      shuffleEncounterDeck
      towers <- dolAmrothObjectivesToSearch
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "outmaneuverTheEnemy.objective"
        causedBy attrs
        targets towers \card -> do
          removeFromEncounterDeck card
          addToStaging card
          doStep 1 (Run [msg, AddToStaging card])
      pure $ OutmaneuverTheEnemy $ attrs {questFlipped = True}
    -- the objective chosen is now in the staging area
    DoStep 1 (Run [Flip (isTarget attrs -> True), AddToStaging card]) -> do
      objectiveWithCardId card.id >>= traverse_ resolveGuarded
      pure q
    EngageEnemy pid eid | attrs.flipped -> do
      ship <- selectAny (EnemyWithId eid <> shipEnemy)
      when ship do
        onCourse <- isOnCourse
        if onCourse
          then dealDamage attrs eid 2
          else do
            ships <- shipObjectivesOf pid
            chooseOrRunOneM pid do
              questionLabeled "outmaneuverTheEnemy.damage"
              causedBy attrs
              targets ships \s -> dealDamage attrs s 2
      OutmaneuverTheEnemy <$> runMessage msg attrs
    _ -> OutmaneuverTheEnemy <$> runMessage msg attrs
