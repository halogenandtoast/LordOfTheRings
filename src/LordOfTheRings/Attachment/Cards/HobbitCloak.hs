module LordOfTheRings.Attachment.Cards.HobbitCloak (hobbitCloak) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype HobbitCloak = HobbitCloak AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Hobbit hero. Limit 1 per hero, on its card def.
hobbitCloak :: AttachmentCard HobbitCloak
hobbitCloak = attachment HobbitCloak Cards.hobbitCloak & attachTo (AttachToHero $ HeroWithTrait Hobbit)

-- | Attached hero gets +2 [defense] while defending against an attack by an enemy with an
-- engagement cost higher than your threat.
instance HasModifiersFor HobbitCloak where
  getModifiersFor (HobbitCloak a) = withHost a \host ->
    modified a host [DefenseBonusAgainstMatching (EnemyWithEngagementCostAboveThreatOf a.controller) 2]
