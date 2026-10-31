module LordOfTheRings.Treachery.Cards.HatefulSearch (hatefulSearch) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HatefulSearch = HatefulSearch TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hatefulSearch :: TreacheryCard HatefulSearch
hatefulSearch = treachery HatefulSearch Cards.hatefulSearch

-- | When Revealed: Deal 1 damage to each location in the staging area. Until the end of the
-- phase, each damaged location gets +1 threat. If there are no locations in the staging
-- area, Hateful Search gains surge.
-- Shadow: Defending player discards a non-objective attachment he controls.
instance RunMessage HatefulSearch where
  runMessage msg t@(HatefulSearch attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      staged <- select LocationInStagingArea
      if null staged
        then revealEncounterCard
        else do
          for_ staged \l -> placeDamage attrs l 1
          doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      damaged <- filterM (fmap (> 0) . getDamage . toTarget) =<< select AnyLocation
      for_ damaged \l -> phaseModifier attrs l (ThreatModifier 1)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndDiscard attrs pid =<< getAttachmentsControlledBy pid
      pure t
    _ -> HatefulSearch <$> runMessage msg attrs
