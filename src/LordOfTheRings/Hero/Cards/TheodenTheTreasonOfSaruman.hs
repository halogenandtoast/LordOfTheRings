module LordOfTheRings.Hero.Cards.TheodenTheTreasonOfSaruman (theodenTheTreasonOfSaruman) where

import LordOfTheRings.Card (CardType (AllyType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Helpers.Modifiers (cardsPlayedThisRound)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype TheodenTheTreasonOfSaruman = TheodenTheTreasonOfSaruman HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on its card def.
theodenTheTreasonOfSaruman :: HeroCard TheodenTheTreasonOfSaruman
theodenTheTreasonOfSaruman = hero TheodenTheTreasonOfSaruman Cards.theodenTheTreasonOfSaruman

-- | Reduce the cost of the first Rohan ally you play from your hand each round by 1 (to a
-- minimum of 0): until you have played one this round.
instance HasModifiersFor TheodenTheTreasonOfSaruman where
  getModifiersFor (TheodenTheTreasonOfSaruman a) = do
    played <- cardsPlayedThisRound
    let rohanAlly code = let def = toCardDef code in cdCardType def == AllyType && Rohan `member` cdCardTraits def
    unless (any (\(p, code, _) -> p == a.owner && rohanAlly code) played)
      $ modified a (PlayerTarget a.owner) [ReduceCostOf (CardWithType AllyType <> CardWithTrait Rohan) 1]
