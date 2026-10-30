module LordOfTheRings.Treachery.Cards.SerpentsTail (serpentsTail) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SerpentsTail = SerpentsTail TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

serpentsTail :: TreacheryCard SerpentsTail
serpentsTail = treachery SerpentsTail Cards.serpentsTail

-- | When Revealed: Deal 1 shadow card to each Ship enemy in play. Then, if you are
-- off-course, each Ship enemy in play gets -10 engagement cost until the end of the round.
instance RunMessage SerpentsTail where
  runMessage msg t@(SerpentsTail attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      ships <- select shipEnemy
      for_ ships dealShadowCard
      whenM isOffCourse $ for_ ships \s -> roundModifier attrs s (EngagementCostModifier (-10))
      pure t
    _ -> SerpentsTail <$> runMessage msg attrs
