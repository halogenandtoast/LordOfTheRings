module LordOfTheRings.Location.Cards.BurningTower (burningTower) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype BurningTower = BurningTower LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

burningTower :: LocationCard BurningTower
burningTower = location BurningTower Cards.burningTower

-- | While Burning Tower is in the staging area, damage cannot be healed.
instance HasModifiersFor BurningTower where
  getModifiersFor (BurningTower a) = when (a.placement == StagingArea) do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [CannotBeHealed]

-- | Forced: After damage is placed on Burning Tower, choose X characters, where X is the
-- amount of damage on Burning Tower. Deal 1 damage to each chosen character.
instance HasAbilities BurningTower where
  getAbilities (BurningTower a) = [mkAbility a 1 $ forced $ DealtDamageWindow After (toTarget a)]

{- | The first player chooses them one at a time, each dealt its damage as it is chosen: the
step counts down those left to choose, its windows the characters already dealt damage.
-}
instance RunMessage BurningTower where
  runMessage msg l@(BurningTower a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      doStep a.damage (UseCardAbility pid (toSource a) 1 [])
      pure l
    DoStep k (UseCardAbility pid (isSource a -> True) 1 chosen) | k > 0 -> do
      lead <- getLead
      characters <- selectCharacters AnyHero AnyAlly
      let left = [c | c <- characters, c `notElem` [t | Window _ (DealtDamage t _) <- chosen]]
      chooseOrRunOneM lead do
        questionLabeled "burningTower.choose"
        causedBy a
        targets left \c -> do
          dealDamage a c 1
          doStep (k - 1) (UseCardAbility pid (toSource a) 1 (mkAfter (DealtDamage c 1) : chosen))
      pure l
    _ -> BurningTower <$> runMessage msg a
