module LordOfTheRings.Treachery.Cards.OldWivesTales (oldWivesTales) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OldWivesTales = OldWivesTales TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

oldWivesTales :: TreacheryCard OldWivesTales
oldWivesTales = treachery OldWivesTales Cards.oldWivesTales

-- | When Revealed: Discard 1 resource from each hero's resource pool, if able. Exhaust any
-- hero that could not discard a resource from its pool.
instance RunMessage OldWivesTales where
  runMessage msg t@(OldWivesTales attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- traverse getHero =<< select AnyHero
      for_ heroes \h -> if h.resources > 0 then removeTokens attrs h Resource 1 else exhaust h
      pure t
    _ -> OldWivesTales <$> runMessage msg attrs
