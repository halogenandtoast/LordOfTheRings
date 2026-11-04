module LordOfTheRings.Treachery.Cards.FearfulShadows (fearfulShadows) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FearfulShadows = FearfulShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fearfulShadows :: TreacheryCard FearfulShadows
fearfulShadows = treachery FearfulShadows Cards.fearfulShadows

-- | Doomed 1.
-- When Revealed: Until the end of the round, treat the printed text box of each character
-- as if it were blank (except for Traits): each in play as it resolves. 'Blank' leaves the
-- traits.
-- Shadow: Defending character cannot ready until the end of the round.
instance RunMessage FearfulShadows where
  runMessage msg t@(FearfulShadows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> roundModifier attrs c Blank
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> roundModifier attrs d CannotReady
      pure t
    _ -> FearfulShadows <$> runMessage msg attrs
