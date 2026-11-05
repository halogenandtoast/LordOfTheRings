module LordOfTheRings.Attachment.Cards.GondolinShield (gondolinShield) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype GondolinShield = GondolinShield AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Ered Mithrin treasure. Attach to a character. Restricted, on its card def.
gondolinShield :: AttachmentCard GondolinShield
gondolinShield = attachment GondolinShield Cards.gondolinShield & attachTo AttachToCharacter

-- | Attached character gets +1 [defense] and gains sentinel.
instance HasModifiersFor GondolinShield where
  getModifiersFor (GondolinShield a) = withHost a \host -> modified a host [DefenseModifier 1, GainKeyword Sentinel]

-- | Response: Exhaust Gondolin Shield and spend 1 resource to cancel a shadow effect just
-- triggered during an attack that attached character is defending. Then, deal the
-- attacking enemy another shadow card.
instance HasAbilities GondolinShield where
  getAbilities (GondolinShield a) =
    [ restricted a 1 shadowEffectsCancelable
        $ ResponseAbility (ShadowEffectAgainstWindow When host) (Costs [exhaustCost a, SpendResourcesCost Neutral 1])
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage GondolinShield where
  runMessage msg s@(GondolinShield a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      cancelShadowEffects windows
      for_ (take 1 [eid | Window _ (ShadowEffectTriggered _ eid) <- windows]) dealShadowCard
      pure s
    _ -> GondolinShield <$> runMessage msg a
