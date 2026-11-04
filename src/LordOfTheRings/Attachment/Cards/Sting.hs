module LordOfTheRings.Attachment.Cards.Sting (sting) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype Sting = Sting AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (enemy), on its card def. Attach to a Hobbit hero. Restricted.
sting :: AttachmentCard Sting
sting = attachment Sting Cards.sting & attachTo (AttachToHero $ HeroWithTrait Hobbit)

-- | Attached hero gets +1 willpower, +1 attack, and +1 defense.
instance HasModifiersFor Sting where
  getModifiersFor (Sting a) = withHost a \host -> modified a host [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]

-- | Response: After attached hero is declared as an attacker or defender against an enemy,
-- deal 1 damage to that enemy.
instance HasAbilities Sting where
  getAbilities (Sting a) =
    concat
      [ [ mkAbility a 1 $ response $ AttackerDeclaredWindow After (Just host)
        , mkAbility a 2 $ response $ DefenderDeclaredWindow After (Just host)
        ]
      | AttachedTo host <- [a.placement]
      ]

instance RunMessage Sting where
  runMessage msg s@(Sting a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (DeclaredAsAttacker _ eid) <- windows]) \eid -> dealDamage a eid 1
      pure s
    UseCardAbility _ (isSource a -> True) 2 windows -> do
      for_ (take 1 [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid -> dealDamage a eid 1
      pure s
    _ -> Sting <$> runMessage msg a
