module LordOfTheRings.Event.Cards.Infighting (infighting) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype Infighting = Infighting EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Move any number of damage from one enemy to another.
infighting :: EventCard Infighting
infighting = eventWith Infighting Cards.infighting (criteriaL .~ exists AnyEnemy)

instance RunMessage Infighting where
  runMessage msg e@(Infighting attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      wounded <- filter ((> 0) . (.damage)) <$> (traverse getEnemy =<< select AnyEnemy)
      chooseOneM pid do
        questionLabeled "infighting.from"
        causedBy attrs
        targets wounded \from -> do
          others <- select (not_ $ EnemyWithId from.id)
          chooseOneM pid do
            questionLabeled "infighting.to"
            causedBy attrs
            targets others \to -> chooseOneM pid do
              questionLabeled "chooseX"
              causedBy attrs
              for_ [1 .. from.damage] \x -> labeledWith "xEquals" [intVar "x" x] do
                removeTokens attrs from Damage x
                dealDamage attrs to x
      pure e
    _ -> Infighting <$> runMessage msg attrs
