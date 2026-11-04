module LordOfTheRings.Attachment.Cards.BlackArrow (blackArrow) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))

newtype BlackArrow = BlackArrow AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Limit 1 per deck, on its card def. Attach to a hero with Ranged.
blackArrow :: AttachmentCard BlackArrow
blackArrow = attachment BlackArrow Cards.blackArrow & attachTo (AttachToHero $ HeroWithKeyword Ranged)

-- | Response: After attached hero declares an attack, add Black Arrow to the victory display
-- to give attached hero +5 [attack] for this attack.
instance HasAbilities BlackArrow where
  getAbilities (BlackArrow a) =
    [ mkAbility a 1 $ response $ CharacterAttackerDeclaredWindow After (HeroWithId hid) (not_ AnyAlly) AnyEnemy
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage BlackArrow where
  runMessage msg s@(BlackArrow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> do
        addToVictoryDisplay a
        createEffect a host [AttackModifier 5] UntilEndOfAttack
      pure s
    _ -> BlackArrow <$> runMessage msg a
