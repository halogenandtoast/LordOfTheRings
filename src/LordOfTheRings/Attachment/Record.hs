{- | The Records of Against the Shadow, as Book of Eldacar: "Attach to a [sphere] hero.
Reduce the cost to play this by 1 for each hero you control with a printed [sphere]
resource icon. Action: Discard this to play any [sphere] event card in your discard pile
as if it were in your hand. Then, place that event on the bottom of your deck."
-}
module LordOfTheRings.Attachment.Record (record, recordModifiers) where

import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Sphere (Sphere)

-- | A Record of the sphere, attached to a hero of it.
record :: IsAttachment a => (AttachmentAttrs -> a) -> CardDef -> Sphere -> AttachmentCard a
record f def sphere = attachment f def & attachTo (AttachToHero $ HeroWithSphere sphere)

{- | In hand it costs less for each of its owner's heroes with the printed icon; attached,
its controller may play the sphere's events from their discard pile by discarding it,
which the player's runner does as such a card is played.
-}
recordModifiers :: HasModifiersM m => Sphere -> AttachmentAttrs -> m ()
recordModifiers sphere a = case a.placement of
  Unplaced -> do
    n <- length <$> select (HeroControlledBy (PlayerWithId a.owner) <> HeroWithPrintedSphere sphere)
    when (n > 0) $ modified a (CardIdTarget a.cardId) [CostModifier (negate n)]
  AttachedTo _ -> modified a (PlayerTarget a.controller) [MayPlayFromDiscard $ CardWithType EventType <> CardWithSphere sphere]
  _ -> pure ()
