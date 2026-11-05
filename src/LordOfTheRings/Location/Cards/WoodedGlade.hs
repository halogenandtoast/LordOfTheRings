module LordOfTheRings.Location.Cards.WoodedGlade (woodedGlade) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype WoodedGlade = WoodedGlade LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

woodedGlade :: LocationCard WoodedGlade
woodedGlade = location WoodedGlade Cards.woodedGlade

instance HasModifiersFor WoodedGlade where
  getModifiersFor (WoodedGlade a) = when (a.placement == ActiveLocation) do
    ordinary <- getsGame (map toTarget . toList . (.entities.attachments))
    conditions <- getConditionAttachments
    for_ (ordNub $ ordinary <> filter nonObjective conditions) \t -> modified a t [Blank]
   where
    nonObjective (ObjectiveTarget _) = False
    nonObjective _ = True

instance RunMessage WoodedGlade where
  runMessage msg l@(WoodedGlade a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      attachments <- filter nonObjective <$> getAttachmentsControlledBy pid
      chooseAndDiscard a pid attachments
      pure l
    _ -> WoodedGlade <$> runMessage msg a
   where
    nonObjective (ObjectiveTarget _) = False
    nonObjective _ = True
