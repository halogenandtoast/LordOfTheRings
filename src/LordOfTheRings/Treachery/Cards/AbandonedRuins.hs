module LordOfTheRings.Treachery.Cards.AbandonedRuins (abandonedRuins) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AbandonedRuins = AbandonedRuins TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

abandonedRuins :: TreacheryCard AbandonedRuins
abandonedRuins = treachery AbandonedRuins Cards.abandonedRuins

-- | When Revealed: Each player resolves the Discover keyword of the active location in
-- turn order. If they cannot, Abandoned Ruins gains surge and doomed 2.
-- They cannot with no active location, or one without the keyword: doomed 2 raises each
-- player's threat by 2 as the keyword does, and surge reveals another card.
-- Shadow: Attacking enemy makes an additional attack against you after this one.
instance RunMessage AbandonedRuins where
  runMessage msg t@(AbandonedRuins attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      active <- select (IsActiveLocation <> LocationWithDiscover)
      case active of
        lid : _ -> eachPlayer \p -> resolveDiscover p lid
        [] -> do
          raiseThreatFromDoomed 2
          revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> AbandonedRuins <$> runMessage msg attrs
