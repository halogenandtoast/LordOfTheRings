module LordOfTheRings.Attachment.Cards.HelmOfHeroes (helmOfHeroes) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype HelmOfHeroes = HelmOfHeroes AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Ered Mithrin treasure. Attach to a character. Restricted, on its card def.
helmOfHeroes :: AttachmentCard HelmOfHeroes
helmOfHeroes = attachment HelmOfHeroes Cards.helmOfHeroes & attachTo AttachToCharacter

-- | Attached character gets +2 [defense].
instance HasModifiersFor HelmOfHeroes where
  getModifiersFor (HelmOfHeroes a) = withHost a \host -> modified a host [DefenseModifier 2]

-- | Response: After attached character takes 1 or more damage from an enemy's attack, add 1
-- resource to any hero's resource pool.
instance HasAbilities HelmOfHeroes where
  getAbilities (HelmOfHeroes a) = [mkAbility a 1 $ response $ DamagedByEnemyAttackWindow After host | AttachedTo host <- [a.placement]]

instance RunMessage HelmOfHeroes where
  runMessage msg s@(HelmOfHeroes a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      chooseOrRunOneM pid do
        questionLabeled "helmOfHeroes.hero"
        causedBy a
        targets heroes \h -> placeResources a h 1
      pure s
    _ -> HelmOfHeroes <$> runMessage msg a
