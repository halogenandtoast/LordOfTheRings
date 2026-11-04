module LordOfTheRings.Treachery.Cards.HeavySnow (heavySnow) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HeavySnow = HeavySnow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 1.
heavySnow :: TreacheryCard HeavySnow
heavySnow = treachery HeavySnow Cards.heavySnow

{- | When Revealed: Exhaust a character. Until the end of the round, characters cannot be
readied by player card effects. The card names no one to choose, so the first player
chooses a ready character in play.
Shadow: Exhaust a character you control.
-}
instance RunMessage HeavySnow where
  runMessage msg t@(HeavySnow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      ready' <- selectCharacters ReadyHero ReadyAlly
      chooseOrRunOneM lead do
        questionLabeled "heavySnow.exhaust"
        causedBy attrs
        targets ready' exhaust
      roundModifier attrs GameTarget CannotBeReadiedByPlayerCards
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust (CardIdSource cid) pid 1
      pure t
    _ -> HeavySnow <$> runMessage msg attrs
