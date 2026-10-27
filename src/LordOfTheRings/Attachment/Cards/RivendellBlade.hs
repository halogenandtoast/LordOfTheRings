module LordOfTheRings.Attachment.Cards.RivendellBlade (rivendellBlade) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype RivendellBlade = RivendellBlade AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noldor or Silvan character. Restricted, as its card def says.
rivendellBlade :: AttachmentCard RivendellBlade
rivendellBlade =
  attachment RivendellBlade Cards.rivendellBlade
    & attachTo (AttachToCharacterMatching (HeroMatchAny [HeroWithTrait Noldor, HeroWithTrait Silvan]) (AllyMatchAny [AllyWithTrait Noldor, AllyWithTrait Silvan]))

-- | When attached character attacks an enemy, that enemy gets -2 defense until the end
-- of the phase.
instance HasAbilities RivendellBlade where
  getAbilities (RivendellBlade a) =
    [mkAbility a 1 $ silent $ AttackerDeclaredWindow After (Just host) | AttachedTo host <- [a.placement]]

instance RunMessage RivendellBlade where
  runMessage msg s@(RivendellBlade a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [eid | Window _ (DeclaredAsAttacker _ eid) <- windows] \eid -> phaseModifier a eid (DefenseModifier (-2))
      pure s
    _ -> RivendellBlade <$> runMessage msg a
