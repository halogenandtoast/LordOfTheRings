module LordOfTheRings.Hero.Cards.Dunhere (dunhere) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Dunhere = Dunhere HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunhere :: HeroCard Dunhere
dunhere = hero Dunhere Cards.dunhere

-- | Dúnhere can target enemies in the staging area when he attacks alone. When doing so,
-- he gets +1 attack.
instance HasModifiersFor Dunhere where
  getModifiersFor (Dunhere a) = modified a a [CanAttackStagingAreaAlone]
