module LordOfTheRings.Ally.Cards.Calphon (calphon) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

newtype Calphon = Calphon AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
calphon :: AllyCard Calphon
calphon = ally Calphon Cards.calphon

-- | Action: Exhaust Calphon to look at the bottom card of your deck. Then, you may switch
-- that card with a card in your hand.
-- If Calphon leaves play, the players lose.
instance HasAbilities Calphon where
  getAbilities (Calphon a) =
    [ mkAbility a 1 $ ActionAbility (exhaustCost a)
    , mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)
    ]

instance RunMessage Calphon where
  runMessage msg s@(Calphon a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      player <- getPlayer pid
      for_ (lastMay player.deck) \bottom -> chooseOneM pid do
        questionLabeled "calphon.switch"
        causedBy a
        showing [bottom]
        labeled "calphon.keep" $ pure ()
        targets player.hand \c -> swapHandWithDeck pid c bottom
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure s
    _ -> Calphon <$> runMessage msg a
