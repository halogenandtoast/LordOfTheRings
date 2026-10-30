module LordOfTheRings.Ally.Cards.LongbeardSentry (longbeardSentry) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))

newtype LongbeardSentry = LongbeardSentry AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

longbeardSentry :: AllyCard LongbeardSentry
longbeardSentry = ally LongbeardSentry Cards.longbeardSentry

-- | Action: Discard 2 cards from the top of your deck to give Longbeard Sentry sentinel
-- and +1 defense until the end of the phase. (Limit once per phase.)
instance HasAbilities LongbeardSentry where
  getAbilities (LongbeardSentry a) =
    [ limited (PlayerLimit PerPhase 1)
        $ restricted a 1 (PlayerExists $ ActivePlayer <> PlayerWithCardsInDeck 2)
        $ ActionAbility Free
    ]

instance RunMessage LongbeardSentry where
  runMessage msg s@(LongbeardSentry a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discardTopOfDeck pid 2
      phaseModifiers a a [GainKeyword Sentinel, DefenseModifier 1]
      pure s
    _ -> LongbeardSentry <$> runMessage msg a
