module LordOfTheRings.Objective.Cards.GavinObjective (gavinObjective) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype GavinObjective = GavinObjective ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Gavin, as he is revealed from the encounter deck, his Planning Action having shuffled
-- him in.
gavinObjective :: ObjectiveCard GavinObjective
gavinObjective = objective GavinObjective Cards.gavin

-- | When Revealed: Put Gavin into play under any player's control committed to the quest
-- (his ally's card does the rest).
instance RunMessage GavinObjective where
  runMessage msg o@(GavinObjective a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      removeFromGame a
      players <- getRemainingPlayers
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "gavin.control"
        causedBy a
        targets players \pid -> putObjectiveAllyIntoPlayUnder pid (MkEncounterCard a.cardId (toCardCode a))
      pure o
    _ -> GavinObjective <$> runMessage msg a
