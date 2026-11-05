module LordOfTheRings.Treachery.Cards.DismayAndFear (dismayAndFear) where

import LordOfTheRings.Card (cdCost, costAsResources, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers (namelessFearThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DismayAndFear = DismayAndFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dismayAndFear :: TreacheryCard DismayAndFear
dismayAndFear = treachery DismayAndFear Cards.dismayAndFear

-- | When Revealed: Choose and remove characters from the quest until the total willpower
-- of characters removed from the quest is equal to or higher than The Nameless Fear's
-- threat. The first player chooses them, one at a time, the willpower still owed the step.
-- Shadow: Discard each attachment you control with a cost less than The Nameless Fear's
-- threat.
instance RunMessage DismayAndFear where
  runMessage msg t@(DismayAndFear attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      owed <- namelessFearThreat
      doStep owed msg
      pure t
    DoStep owed revealed@(WhenRevealed (isTarget attrs -> True)) | owed > 0 -> do
      lead <- getLead
      questing <- selectCharacters CommittedHero CommittedAlly
      chooseOneM lead do
        questionLabeled "dismayAndFear.remove"
        causedBy attrs
        targets questing \c -> do
          willpower <- getWillpower c
          removeFromQuest c
          doStep (owed - willpower) revealed
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      x <- namelessFearThreat
      attachments <- getsGame (toList . (.entities.attachments))
      for_ attachments \a -> do
        let cost = maybe 0 costAsResources (cdCost $ toCardDef a)
        when ((toAttrs a).controller == pid && isAttached a.placement && cost < x) $ discard attrs a
      pure t
    _ -> DismayAndFear <$> runMessage msg attrs
   where
    isAttached = \case
      AttachedTo _ -> True
      _ -> False
