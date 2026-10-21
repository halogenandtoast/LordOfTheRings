module LordOfTheRings.Event.Cards.ALightInTheDark (aLightInTheDark) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype ALightInTheDark = ALightInTheDark EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose an enemy engaged with a player. Return that enemy to the staging area.
aLightInTheDark :: EventCard ALightInTheDark
aLightInTheDark = eventWith ALightInTheDark Cards.aLightInTheDark (criteriaL .~ exists (EnemyEngagedWith AnyPlayer))

instance RunMessage ALightInTheDark where
  runMessage msg e@(ALightInTheDark attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      engaged <- select (EnemyEngagedWith AnyPlayer)
      chooseOneM pid do
        questionLabeled "aLightInTheDark.return"
        causedBy attrs
        targets engaged \eid -> placeEnemy eid StagingArea
      pure e
    _ -> ALightInTheDark <$> runMessage msg attrs
