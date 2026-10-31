module LordOfTheRings.Ally.Cards.Mendor (mendor) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

newtype Mendor = Mendor AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective-ally of the Revised Core campaign: the first player gains control of him.
mendor :: AllyCard Mendor
mendor = ally Mendor Cards.mendor

-- | Response: After a quest card is defeated, ready Mendor. Then, each player draws 1 card.
instance HasAbilities Mendor where
  getAbilities (Mendor a) = [mkAbility a 1 $ response $ AnyQuestDefeatedWindow After]

-- | If Mendor leaves play, remove him from the game. Each of the campaign's cards: "If
-- Mendor leaves play, the players lose the game."
instance RunMessage Mendor where
  runMessage msg s@(Mendor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      eachPlayer \pid -> drawCards a pid 1
      pure s
    Discard (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> Mendor <$> runMessage msg a
