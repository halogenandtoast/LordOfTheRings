module LordOfTheRings.Treachery.Cards.BitterWind (bitterWind) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BitterWind = BitterWind TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bitterWind :: TreacheryCard BitterWind
bitterWind = treachery BitterWind Cards.bitterWind

-- | When Revealed: The first player must discard 3 resources from each hero he controls.
-- Shadow: Defending player must discard 2 resources from each hero he controls.
instance RunMessage BitterWind where
  runMessage msg t@(BitterWind attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      discardFromEachHero lead 3
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardFromEachHero pid 2
      pure t
    _ -> BitterWind <$> runMessage msg attrs
   where
    discardFromEachHero pid n = do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      for_ heroes \h -> removeTokens attrs h Resource n
