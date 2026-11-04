module LordOfTheRings.Hero.Cards.MerryTheBlackRiders (merryTheBlackRiders) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype MerryTheBlackRiders = MerryTheBlackRiders HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

merryTheBlackRiders :: HeroCard MerryTheBlackRiders
merryTheBlackRiders = hero MerryTheBlackRiders Cards.merryTheBlackRiders

-- | Merry gets +1 [attack] for each Hobbit hero you control.
instance HasModifiersFor MerryTheBlackRiders where
  getModifiersFor (MerryTheBlackRiders a) = do
    hobbits <- length <$> select (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Hobbit)
    modifySelf a [AttackModifier hobbits]

-- | Response: After Merry participates in an attack that destroys an enemy, ready another
-- character that participated in that attack.
instance HasAbilities MerryTheBlackRiders where
  getAbilities (MerryTheBlackRiders a) = [mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just $ toTarget a)]

instance RunMessage MerryTheBlackRiders where
  runMessage msg h@(MerryTheBlackRiders attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 windows -> do
      let others = nub [c | Window _ (EnemyDestroyedByAttack _ attackers _) <- windows, c <- attackers, c /= toTarget attrs]
      chooseOneM pid do
        questionLabeled "merryTheBlackRiders.ready"
        causedBy attrs
        targets others ready
      pure h
    _ -> MerryTheBlackRiders <$> runMessage msg attrs
