module LordOfTheRings.Hero.Cards.Faramir (faramir) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Faramir = Faramir HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

faramir :: HeroCard Faramir
faramir = hero Faramir Cards.faramir

-- | Faramir gets +1 attack for each enemy in the staging area.
instance HasModifiersFor Faramir where
  getModifiersFor (Faramir a) = do
    n <- length <$> select EnemyInStagingArea
    when (n > 0) $ modifySelf a [AttackModifier n]
