module LordOfTheRings.Treachery.Cards.Snowstorm (snowstorm) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Snowstorm = Snowstorm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

snowstorm :: TreacheryCard Snowstorm
snowstorm = treachery Snowstorm Cards.snowstorm

-- | When Revealed: Each questing character gets -1 [Willpower] until the end of the phase.
-- Shadow: Until the end of the phase, characters defending this attack get -1 [Willpower]
-- and are discarded if their [Willpower] is 0.
instance RunMessage Snowstorm where
  runMessage msg t@(Snowstorm attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      questing <- selectCharacters CommittedHero CommittedAlly
      for_ questing \c -> phaseModifier attrs c (WillpowerModifier (-1))
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> phaseModifiers attrs d [WillpowerModifier (-1), DiscardedAtNoWillpower]
      pure t
    _ -> Snowstorm <$> runMessage msg attrs
