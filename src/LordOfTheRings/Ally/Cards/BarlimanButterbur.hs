module LordOfTheRings.Ally.Cards.BarlimanButterbur (barlimanButterbur) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Hobbit))

newtype BarlimanButterbur = BarlimanButterbur AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

barlimanButterbur :: AllyCard BarlimanButterbur
barlimanButterbur = ally BarlimanButterbur Cards.barlimanButterbur

-- | If each hero you control has the Hobbit trait, damage from undefended attacks against
-- you may be assigned to Barliman Butterbur.
instance HasModifiersFor BarlimanButterbur where
  getModifiersFor (BarlimanButterbur a) = for_ (placementController a.placement) \pid -> do
    others <- selectAny (HeroControlledBy (PlayerWithId pid) <> NotHero (HeroWithTrait Hobbit))
    unless others $ modified a (PlayerTarget pid) [UndefendedDamageMayGoTo (toTarget a)]
