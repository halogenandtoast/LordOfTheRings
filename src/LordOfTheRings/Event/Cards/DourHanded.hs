module LordOfTheRings.Event.Cards.DourHanded (dourHanded) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype DourHanded = DourHanded EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

dourHanded :: EventCard DourHanded
dourHanded = eventWith DourHanded Cards.dourHanded \a -> a & criteriaL .~ exists (EnemyEngagedWith $ PlayerWithId a.owner)

-- | Action: Deal X damage to an enemy engaged with you. X is the number of side quests in
-- the victory display.
instance RunMessage DourHanded where
  runMessage msg e@(DourHanded attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      x <- getSideQuestsInVictoryDisplay
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy attrs
        targets enemies \eid -> dealDamage attrs eid x
      pure e
    _ -> DourHanded <$> runMessage msg attrs
