module LordOfTheRings.Event.Cards.ArrowsFromTheTrees (arrowsFromTheTrees) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype ArrowsFromTheTrees = ArrowsFromTheTrees EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only after making engagement checks.
-- Encounter Action: If no enemies were engaged this phase, deal 1 damage to each enemy in
-- the staging area. Then, the players as a group may spend 3 tactics resources to deal 2
-- additional damage to each enemy in the staging area.
arrowsFromTheTrees :: EventCard ArrowsFromTheTrees
arrowsFromTheTrees =
  eventWith ArrowsFromTheTrees Cards.arrowsFromTheTrees \a ->
    a & criteriaL .~ DuringPhase EncounterPhase <> TargetHasModifier GameTarget AfterEngagementChecks

instance RunMessage ArrowsFromTheTrees where
  runMessage msg e@(ArrowsFromTheTrees attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      engaged <- elem EnemyEngagedThisPhase <$> getModifiers GameTarget
      enemies <- select (EnemyInStagingArea <> EnemyNotImmuneToPlayerCards)
      unless engaged do
        for_ enemies \eid -> dealDamage attrs eid 1
        tactics <- getGroupResourcesOfSphere Tactics
        when (tactics >= 3 && notNull enemies) $ chooseOneM pid do
          questionLabeled "arrowsFromTheTrees.spend"
          causedBy attrs
          labeled "arrowsFromTheTrees.damage" do
            spendGroupResources pid Tactics 3
            -- not those the first damage destroyed
            for_ enemies \eid -> whileInPlay eid $ dealDamage attrs eid 2
          labeled "arrowsFromTheTrees.done" (pure ())
      pure e
    _ -> ArrowsFromTheTrees <$> runMessage msg attrs
