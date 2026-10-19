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
      pushAll
        $ if null enemies && null locations
          then [RevealEncounterCard]
          else [CreateEffect $ Effect (toSource attrs) c [ThreatModifier 1] UntilEndOfPhase | c <- enemies <> locations]
      pure t
    ResolveShadowEffect cid _ pid defender | cid == attrs.cardId -> do
      case defender of
        Just character -> push . ChooseAndDiscard pid =<< getAttachmentsOn character
        Nothing -> pushAll . map Discard =<< getAttachmentsControlledBy pid
      pure t
    _ -> DrivenByShadow <$> runMessage msg attrs
