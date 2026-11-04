module LordOfTheRings.Hero.Cards.BrandSonOfBainLeadership (brandSonOfBainLeadership) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Dale))

newtype BrandSonOfBainLeadership = BrandSonOfBainLeadership HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

brandSonOfBainLeadership :: HeroCard BrandSonOfBainLeadership
brandSonOfBainLeadership = hero BrandSonOfBainLeadership Cards.brandSonOfBainLeadership

-- | Each Dale character you control with a player attachment gets +1 willpower.
instance HasModifiersFor BrandSonOfBainLeadership where
  getModifiersFor (BrandSonOfBainLeadership a) = do
    let you = PlayerWithId a.owner
    dale <-
      selectCharacters
        (HeroControlledBy you <> HeroWithTrait Dale <> HeroWithPlayerAttachment)
        (AllyControlledBy you <> AllyWithTrait Dale <> AllyWithPlayerAttachment)
    for_ dale \c -> modified a c [WillpowerModifier 1]

-- | Response: After you play an attachment on a Dale character without an attachment, draw
-- a card.
instance HasAbilities BrandSonOfBainLeadership where
  getAbilities (BrandSonOfBainLeadership a) =
    [mkAbility a 1 $ response $ AttachmentPlayedOnBareCharacterWindow After a.owner (HeroWithTrait Dale) (AllyWithTrait Dale)]

instance RunMessage BrandSonOfBainLeadership where
  runMessage msg h@(BrandSonOfBainLeadership attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      drawCards attrs pid 1
      pure h
    _ -> BrandSonOfBainLeadership <$> runMessage msg attrs
