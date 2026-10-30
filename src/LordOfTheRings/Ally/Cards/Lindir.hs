module LordOfTheRings.Ally.Cards.Lindir (lindir) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Lindir = Lindir AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lindir :: AllyCard Lindir
lindir = ally Lindir Cards.lindir

-- | Response: After Lindir enters play, if you have less than 3 cards in your hand, draw
-- until you have 3 cards in your hand.
instance HasAbilities Lindir where
  getAbilities (Lindir a) =
    [ restricted a 1 (NotCriterion $ PlayerExists $ PlayerWithId a.owner <> PlayerWithCardsInHand 3)
        $ response
        $ EntersPlayWindow After (toTarget a)
    ]

instance RunMessage Lindir where
  runMessage msg s@(Lindir a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      held <- length . (.hand) <$> getPlayer pid
      when (held < 3) $ drawCards a pid (3 - held)
      pure s
    _ -> Lindir <$> runMessage msg a
