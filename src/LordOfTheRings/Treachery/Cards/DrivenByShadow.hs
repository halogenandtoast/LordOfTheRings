module LordOfTheRings.Treachery.Cards.DrivenByShadow (drivenByShadow) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DrivenByShadow = DrivenByShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

drivenByShadow :: TreacheryCard DrivenByShadow
drivenByShadow = treachery DrivenByShadow Cards.drivenByShadow

instance RunMessage DrivenByShadow where
  runMessage msg t@(DrivenByShadow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- map toTarget <$> select EnemyInStagingArea
      locations <- map toTarget <$> select LocationInStagingArea
      if null enemies && null locations
        then revealEncounterCard
        else for_ (enemies <> locations) \c -> phaseModifier attrs c (ThreatModifier 1)
      pure t
    ResolveShadowEffect cid eid pid defender | cid == attrs.cardId -> do
      case defender of
        Just character -> chooseAndDiscard (EnemySource eid) pid =<< getAttachmentsOn character
        Nothing -> traverse_ discard =<< getAttachmentsControlledBy pid
      pure t
    _ -> DrivenByShadow <$> runMessage msg attrs
