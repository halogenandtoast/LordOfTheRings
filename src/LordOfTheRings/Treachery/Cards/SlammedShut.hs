module LordOfTheRings.Treachery.Cards.SlammedShut (slammedShut) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SlammedShut = SlammedShut TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

slammedShut :: TreacheryCard SlammedShut
slammedShut = treachery SlammedShut Cards.slammedShut

-- | Doomed 1.
-- When Revealed: Each player chooses and exhausts a character he controls. Until the end of
-- the round, characters cannot be readied by player card effects.
-- The printed card (Hall of Beorn's scan of TGoF 108) reads "characters cannot be readied
-- by player card effects": every character, not only the one exhausted.
instance RunMessage SlammedShut where
  runMessage msg t@(SlammedShut attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> chooseAndExhaust attrs p 1
      roundModifier attrs GameTarget CannotBeReadiedByPlayerCards
      pure t
    _ -> SlammedShut <$> runMessage msg attrs
