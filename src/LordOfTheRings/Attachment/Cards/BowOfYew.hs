module LordOfTheRings.Attachment.Cards.BowOfYew (bowOfYew) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dale, Warrior))

newtype BowOfYew = BowOfYew AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dale or Warrior character. Restricted.
bowOfYew :: AttachmentCard BowOfYew
bowOfYew =
  attachment BowOfYew Cards.bowOfYew
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Dale, HeroWithTrait Warrior])
          (AllyMatchAny [AllyWithTrait Dale, AllyWithTrait Warrior])
      )

-- | Response: After attached character is declared as an attacker, exhaust Bow of Yew to
-- deal 1 damage to the defending enemy.
instance HasAbilities BowOfYew where
  getAbilities (BowOfYew a) =
    [ mkAbility a 1 $ ResponseAbility (AttackerDeclaredWindow After (Just host)) (exhaustCost a)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage BowOfYew where
  runMessage msg s@(BowOfYew a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (DeclaredAsAttacker t eid) <- windows, AttachedTo t == a.placement]) \eid ->
        dealDamage a eid 1
      pure s
    _ -> BowOfYew <$> runMessage msg a
