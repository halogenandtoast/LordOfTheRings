module LordOfTheRings.Attachment.Cards.Anduril (anduril) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (immuneToSource)
import LordOfTheRings.Trait (Trait (Noble))

newtype Anduril = Anduril AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Road Darkens. Attach to a Noble hero or Aragorn. Restricted, on its card
-- def.
anduril :: AttachmentCard Anduril
anduril = attachment Anduril Cards.anduril & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Noble, HeroWithTitle "Aragorn"])

-- | Attached hero gets +1 [willpower], +1 [attack], and +1 [defense].
instance HasModifiersFor Anduril where
  getModifiersFor (Anduril a) = withHost a \host -> modified a host [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]

-- | Response: After an attack in which the attached hero defended resolves, exhaust Andúril
-- to target the enemy that just attacked. Declare attached hero as an attacker against
-- that enemy (and resolve the attack). The card declares him, ready or exhausted.
instance HasAbilities Anduril where
  getAbilities (Anduril a) =
    [mkAbility a 1 $ ResponseAbility (DefendedAttackWindow After (Just host)) (exhaustCost a) | AttachedTo host <- [a.placement]]

instance RunMessage Anduril where
  runMessage msg s@(Anduril a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ [host | AttachedTo host <- [a.placement]] \host ->
        for_ (take 1 [eid | Window _ (DefendedAttack _ eid) <- windows]) \eid ->
          unlessM (immuneToSource (toSource a) eid) $ attackEnemyWith pid a host eid 0
      pure s
    _ -> Anduril <$> runMessage msg a
