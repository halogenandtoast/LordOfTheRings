module LordOfTheRings.Event.Cards.IslandAmidPerils (islandAmidPerils) where

import LordOfTheRings.Card (CardCost (StaticCost), cdCost, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype IslandAmidPerils = IslandAmidPerils EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Return a Silvan ally you control to your hand to reduce your threat by X where
-- X is the printed cost of the ally returned to your hand.
islandAmidPerils :: EventCard IslandAmidPerils
islandAmidPerils =
  eventWith IslandAmidPerils Cards.islandAmidPerils \a -> a & criteriaL .~ controlsAlly a.owner (AllyWithTrait Silvan)

instance RunMessage IslandAmidPerils where
  runMessage msg e@(IslandAmidPerils attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Silvan)
      chooseOrRunOneM pid do
        questionLabeled "returnAllyToHand"
        causedBy attrs
        targets allies \aid -> do
          printed <- toCardDef <$> getAlly aid
          returnToHand aid
          lowerThreat pid $ case cdCost printed of
            Just (StaticCost n) -> n
            _ -> 0
      pure e
    _ -> IslandAmidPerils <$> runMessage msg attrs
