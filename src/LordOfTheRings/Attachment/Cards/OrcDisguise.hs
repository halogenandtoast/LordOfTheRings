module LordOfTheRings.Attachment.Cards.OrcDisguise (orcDisguise) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Fellowship (ringBearer)
import LordOfTheRings.Trait (Trait (Orc))

newtype OrcDisguise = OrcDisguise AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to the Ring-bearer. Limit one per hero, on its card def.
orcDisguise :: AttachmentCard OrcDisguise
orcDisguise = attachment OrcDisguise Cards.orcDisguise & attachTo (AttachToHero ringBearer)

-- | While an Orc is making an engagement check with you, it gets +5 engagement cost.
instance HasModifiersFor OrcDisguise where
  getModifiersFor (OrcDisguise a) = case a.placement of
    AttachedTo _ -> do
      orcs <- select (EnemyWithTrait Orc)
      for_ orcs \eid -> modified a eid [EngagementCostAgainst a.controller 5]
    _ -> pure ()

-- | Response: After an Orc engages you, discard Orc Disguise. Until the end of the round,
-- that enemy cannot attack you.
instance HasAbilities OrcDisguise where
  getAbilities (OrcDisguise a) =
    [ mkAbility a 1 $ response $ PlayerEngagedMatchingEnemyWindow After a.controller (EnemyWithTrait Orc)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage OrcDisguise where
  runMessage msg s@(OrcDisguise a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      discard a a
      orcs <- filterM (\eid -> selectAny (EnemyWithId eid <> EnemyWithTrait Orc)) [eid | Window _ (EnemyEngaged eid p) <- windows, p == pid]
      for_ (take 1 orcs) \eid -> roundModifier a eid (CannotAttackPlayer pid)
      pure s
    _ -> OrcDisguise <$> runMessage msg a
