{- | Wild Stallion as the Mount attachment its Planning Action makes it: the ally card,
attached. It is built from the ally's card, under the ally's code, as an ally that gains the
hero card type is built from its own (see 'LordOfTheRings.Hero.AllyHero'); the text it
counts as having is here.
-}
module LordOfTheRings.Attachment.Cards.WildStallion (wildStallion) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Restricted))
import LordOfTheRings.Trait (Trait (Mount))

newtype WildStallion = WildStallion AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attached to an ally only by its Planning Action, which checks Limit 1 per ally.
wildStallion :: AttachmentCard WildStallion
wildStallion = attachment WildStallion Allies.wildStallion & attachTo (AttachToAlly AnyAlly)

-- | Counts as a Mount attachment with the text: "Restricted. Limit 1 per ally. Attached
-- ally gets +1 [willpower], +1 [attack], +1 [defense], and +1 hit point."
instance HasModifiersFor WildStallion where
  getModifiersFor (WildStallion a) = do
    modifySelf a [GainTrait Mount, GainKeyword Restricted]
    withHost a \host -> modified a host [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1, HitPointModifier 1]
