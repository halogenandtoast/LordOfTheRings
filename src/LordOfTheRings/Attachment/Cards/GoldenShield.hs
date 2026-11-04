module LordOfTheRings.Attachment.Cards.GoldenShield (goldenShield) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Trait (Trait (Rohan))

newtype GoldenShield = GoldenShield AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Rohan hero. Restricted.
goldenShield :: AttachmentCard GoldenShield
goldenShield = attachment GoldenShield Cards.goldenShield & attachTo (AttachToHero $ HeroWithTrait Rohan)

-- | Response: After attached hero is declared as a defender, exhaust Golden Shield to add
-- attached hero's willpower to its defense for this attack.
instance HasAbilities GoldenShield where
  getAbilities (GoldenShield a) =
    [ mkAbility a 1 $ ResponseAbility (DefenderDeclaredWindow After (Just host)) (exhaustCost a)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage GoldenShield where
  runMessage msg s@(GoldenShield a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> do
        -- its willpower as the effect resolves
        x <- getWillpower host
        when (x > 0) $ attackModifier a host (DefenseModifier x)
      pure s
    _ -> GoldenShield <$> runMessage msg a
