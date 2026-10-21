module LordOfTheRings.Hero.Cards.BrandSonOfBain (brandSonOfBain) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype BrandSonOfBain = BrandSonOfBain HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

brandSonOfBain :: HeroCard BrandSonOfBain
brandSonOfBain = hero BrandSonOfBain Cards.brandSonOfBain

-- | Ranged.
-- Response: After Brand son of Bain attacks and defeats an enemy engaged with another
-- player, choose and ready one of that player's characters.
instance HasAbilities BrandSonOfBain where
  getAbilities (BrandSonOfBain a) =
    [mkAbility a 1 $ response $ EnemyOfOtherPlayerDestroyedByAttackWindow After a.owner (toTarget a)]

instance RunMessage BrandSonOfBain where
  runMessage msg h@(BrandSonOfBain attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 windows -> do
      for_ (headMay [p | Window _ (EngagedEnemyDestroyedByAttack p _) <- windows]) \other -> do
        exhausted <- selectCharacters (HeroControlledBy (PlayerWithId other) <> ExhaustedHero) (AllyControlledBy (PlayerWithId other) <> ExhaustedAlly)
        chooseOrRunOneM pid do
          questionLabeled "brandSonOfBain.ready"
          causedBy attrs
          targets exhausted ready
      pure h
    _ -> BrandSonOfBain <$> runMessage msg attrs
