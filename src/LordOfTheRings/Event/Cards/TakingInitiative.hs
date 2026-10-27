module LordOfTheRings.Event.Cards.TakingInitiative (takingInitiative) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Card (costAsResources)

newtype TakingInitiative = TakingInitiative EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Discard the top card of your deck. If the discarded card's printed cost is
-- equal to or higher than the number of characters you control, draw 2 cards and deal 2
-- damage to any enemy.
takingInitiative :: EventCard TakingInitiative
takingInitiative = event TakingInitiative Cards.takingInitiative

instance RunMessage TakingInitiative where
  runMessage msg e@(TakingInitiative attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      top <- headMay . (.deck) <$> getPlayer pid
      characters <- length <$> charactersControlledBy pid
      discardTopOfDeck pid 1
      for_ top \c -> when (maybe 0 costAsResources c.cost >= characters) do
        drawCards pid 2
        enemies <- select AnyEnemy
        chooseOneM pid do
          questionLabeled "takingInitiative.damage"
          causedBy attrs
          targets enemies \en -> dealDamage attrs en 2
      pure e
    _ -> TakingInitiative <$> runMessage msg attrs
