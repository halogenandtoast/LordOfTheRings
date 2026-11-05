module LordOfTheRings.Hero.Cards.DenethorLeadership (denethorLeadership) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype DenethorLeadership = DenethorLeadership HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

denethorLeadership :: HeroCard DenethorLeadership
denethorLeadership = hero DenethorLeadership Cards.denethorLeadership

otherGondorHeroes :: HeroAttrs -> HeroMatcher
otherGondorHeroes a = HeroWithTrait Gondor <> not_ (HeroWithId a.id)

-- | Action: Move 1 resource from Denethor's resource pool to another Gondor hero's
-- resource pool. (Limit once per round.)
instance HasAbilities DenethorLeadership where
  getAbilities (DenethorLeadership a) =
    [ limited (GroupLimit PerRound 1)
        $ restricted a 1 (exists (HeroWithId a.id <> HeroWithResources) <> exists (otherGondorHeroes a))
        $ ActionAbility Free
    ]

instance RunMessage DenethorLeadership where
  runMessage msg h@(DenethorLeadership attrs) = case msg of
    -- Setup: Add 2 resources to Denethor's resource pool.
    DoStep 3 Setup -> do
      placeResources attrs attrs 2
      DenethorLeadership <$> runMessage msg attrs
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (otherGondorHeroes attrs)
      chooseOrRunOneM pid do
        questionLabeled "denethorLeadership.moveTo"
        causedBy attrs
        targets heroes \other -> do
          moveResourcesFromHero attrs attrs.id other 1
      pure h
    _ -> DenethorLeadership <$> runMessage msg attrs
