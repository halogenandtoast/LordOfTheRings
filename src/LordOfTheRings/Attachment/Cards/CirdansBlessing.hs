module LordOfTheRings.Attachment.Cards.CirdansBlessing (
  cirdansBlessingLeadership,
  cirdansBlessingTactics,
  cirdansBlessingSpirit,
  cirdansBlessingLore,
) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Card.Code (CardCode (..))
import LordOfTheRings.Helpers.Campaign (campaignLevel)

-- | Círdan's Blessing, a scalable boon of the Dream-chaser campaign, one for each sphere.
newtype CirdansBlessing = CirdansBlessing AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a character.
cirdansBlessingLeadership :: AttachmentCard CirdansBlessing
cirdansBlessingLeadership = attachment CirdansBlessing Cards.cirdansBlessingLeadership & attachTo AttachToCharacter

cirdansBlessingTactics :: AttachmentCard CirdansBlessing
cirdansBlessingTactics = attachment CirdansBlessing Cards.cirdansBlessingTactics & attachTo AttachToCharacter

cirdansBlessingSpirit :: AttachmentCard CirdansBlessing
cirdansBlessingSpirit = attachment CirdansBlessing Cards.cirdansBlessingSpirit & attachTo AttachToCharacter

-- | Attach to a hero.
cirdansBlessingLore :: AttachmentCard CirdansBlessing
cirdansBlessingLore = attachment CirdansBlessing Cards.cirdansBlessingLore

sphereOf :: AttachmentAttrs -> Text
sphereOf a = case unCardCode (toCardCode a) of
  "DCC205" -> "leadership"
  "DCC206" -> "tactics"
  "DCC207" -> "spirit"
  _ -> "lore"

{- | Leadership: Response: When attached character defends, remove 1 resource from Círdan's
Blessing to give that character +3 defense for this attack.
Tactics: Response: When attached character attacks, remove 1 resource from Círdan's
Blessing to give that character +3 attack for this attack.
Spirit: Action: Exhaust Círdan's Blessing and remove 1 resource from it to give attached
hero +3 willpower until the end of the phase.
Lore: Response: After the active location is explored, move 1 resource from Círdan's
Blessing to attached hero's resource pool. Then, heal 1 damage from attached hero.
-}
instance HasAbilities CirdansBlessing where
  getAbilities (CirdansBlessing a) = mkAbility a 2 (silent $ entersPlay a) : case a.placement of
    AttachedTo host | countTokens Resource (attachmentTokens a) > 0 -> case sphereOf a of
      "leadership" -> [mkAbility a 1 $ response $ DefenderDeclaredWindow After (Just host)]
      "tactics" -> [mkAbility a 1 $ response $ AttackerDeclaredWindow After (Just host)]
      "spirit" -> [mkAbility a 1 $ ActionAbility (exhaustCost a)]
      _ -> [mkAbility a 1 $ response $ ActiveLocationExploredWindow After]
    _ -> []

instance RunMessage CirdansBlessing where
  runMessage msg s@(CirdansBlessing a) = case msg of
    -- Círdan's Blessing enters play with X resources on it. X is 1 more than its level.
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      x <- (+ 1) <$> campaignLevel a
      placeResources a a x
      pure s
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeTokens a a Resource 1
      withHost a \host -> case sphereOf a of
        "leadership" -> attackModifier a host (DefenseModifier 3)
        "tactics" -> attackModifier a host (AttackModifier 3)
        "spirit" -> phaseModifier a host (WillpowerModifier 3)
        _ -> do
          placeResources a host 1
          healDamage a host 1
      pure s
    _ -> CirdansBlessing <$> runMessage msg a
