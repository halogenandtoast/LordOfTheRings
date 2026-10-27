module LordOfTheRings.Event.Cards.HailOfStones (hailOfStones) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype HailOfStones = HailOfStones EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust X characters to deal X damage to an enemy in the staging area.
hailOfStones :: EventCard HailOfStones
hailOfStones = eventWith HailOfStones Cards.hailOfStones (criteriaL .~ exists EnemyInStagingArea)

-- | Each character exhausted comes back as a later step, counting X, until the player is
-- done.
instance RunMessage HailOfStones where
  runMessage msg e@(HailOfStones attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      doStep 0 msg
      pure e
    DoStep x step@(PlayEvent pid card _) | card.id == toId attrs -> do
      let you = PlayerWithId pid
      characters <- selectCharacters (HeroControlledBy you <> ReadyHero) (AllyControlledBy you <> ReadyAlly)
      enemies <- select EnemyInStagingArea
      chooseOneM pid do
        questionLabeledWith "hailOfStones.exhaust" [intVar "count" x]
        causedBy attrs
        targets characters \c -> do
          exhaust c
          doStep (x + 1) step
        labeled "hailOfStones.done" $ when (x > 0) $ chooseOneM pid do
          questionLabeledWith "hailOfStones.chooseEnemy" [intVar "count" x]
          causedBy attrs
          targets enemies \en -> dealDamage attrs en x
      pure e
    _ -> HailOfStones <$> runMessage msg attrs
