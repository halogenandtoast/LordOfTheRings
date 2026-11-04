module LordOfTheRings.Objective.Cards.UnderTheWatchfulEye (underTheWatchfulEye) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

-- | Of the Power of Mordor deck (The Temple of Doom, The Fortress of Nurn).
newtype UnderTheWatchfulEye = UnderTheWatchfulEye ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underTheWatchfulEye :: ObjectiveCard UnderTheWatchfulEye
underTheWatchfulEye = objective UnderTheWatchfulEye Cards.underTheWatchfulEye

-- | Forced: After a player draws any number of cards, he raises his threat by 1.
instance HasAbilities UnderTheWatchfulEye where
  getAbilities (UnderTheWatchfulEye a) = [mkAbility a 1 $ forced $ DrewCardsWindow After Nothing]

-- | When Revealed: Each player with more than 5 cards in his hand discards cards from his
-- hand until he has only 5.
instance RunMessage UnderTheWatchfulEye where
  runMessage msg o@(UnderTheWatchfulEye attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        inHand <- length . (.hand) <$> getPlayer pid
        when (inHand > 5) $ chooseAndDiscardFromHand pid attrs (inHand - 5)
      pure o
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (CardsDrawn pid _) <- windows] \pid -> encounterRaisesThreat pid 1
      pure o
    _ -> UnderTheWatchfulEye <$> runMessage msg attrs
