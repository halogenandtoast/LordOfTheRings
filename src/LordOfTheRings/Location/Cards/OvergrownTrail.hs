module LordOfTheRings.Location.Cards.OvergrownTrail (overgrownTrail) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger))

newtype OvergrownTrail = OvergrownTrail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

overgrownTrail :: LocationCard OvergrownTrail
overgrownTrail = location OvergrownTrail Cards.overgrownTrail

-- | Action: Exhaust a Ranger character to place 3 progress tokens on Overgrown Trail.
instance HasAbilities OvergrownTrail where
  getAbilities (OvergrownTrail a) =
    [anyPlayer $ mkAbility a 1 $ ActionAbility $ ExhaustCharacterCost rangerHero rangerAlly]
   where
    rangerHero = HeroControlledBy ActivePlayer <> HeroWithTrait Ranger
    rangerAlly = AllyControlledBy ActivePlayer <> AllyWithTrait Ranger

-- | Shadow: Remove X progress tokens from the current quest, where X is the attacking
-- enemy's threat.
instance RunMessage OvergrownTrail where
  runMessage msg l@(OvergrownTrail a) = case msg of
    -- the Ranger is exhausted as the cost is paid
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgress a a 3
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      removeProgressFromQuest a =<< getEnemyThreat eid
      pure l
    _ -> OvergrownTrail <$> runMessage msg a
