module LordOfTheRings.Attachment.Cards.OLorien (oLorien) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Silvan))

newtype OLorien = OLorien AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

oLorien :: AttachmentCard OLorien
oLorien = attachment OLorien Cards.oLorien

-- | Attach to a hero. Action: Exhaust O Lórien! to lower the cost of the next Silvan ally
-- played this phase by 1 (to a minimum of 1).
--
-- ponytail: only the next one its controller plays; with other players at the table the
-- next one any of them plays should be.
instance HasAbilities OLorien where
  getAbilities (OLorien a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a) | AttachedTo _ <- [a.placement]]

instance RunMessage OLorien where
  runMessage msg s@(OLorien a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      phaseModifier a (PlayerTarget pid) (ReduceNextCostOf (CardWithType AllyType <> CardWithTrait Silvan) 1)
      pure s
    _ -> OLorien <$> runMessage msg a
