module LordOfTheRings.Location.Cards.UmbarStreet (umbarStreet) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype UmbarStreet = UmbarStreet LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarStreet :: LocationCard UmbarStreet
umbarStreet = location UmbarStreet Cards.umbarStreet

-- | While Umbar Street is the active location, each enemy gets -1 engagement cost for each
-- progress token on the main quest.
instance HasModifiersFor UmbarStreet where
  getModifiersFor (UmbarStreet a) = when (a.placement == ActiveLocation) do
    n <- getMainQuestProgress
    when (n > 0) do
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [EngagementCostModifier (-n)]

-- | Shadow: Discard an attachment you control.
instance RunMessage UmbarStreet where
  runMessage msg l@(UmbarStreet a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard a pid attachments
      pure l
    _ -> UmbarStreet <$> runMessage msg a
