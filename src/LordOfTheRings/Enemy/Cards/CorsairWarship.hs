module LordOfTheRings.Enemy.Cards.CorsairWarship (corsairWarship) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CorsairWarship = CorsairWarship EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairWarship :: EnemyCard CorsairWarship
corsairWarship = enemy CorsairWarship Cards.corsairWarship

-- | Boarding 3. Cannot have attachments. While Corsair Warship is in the staging area, it
-- cannot be damaged and gains archery 4.
instance HasModifiersFor CorsairWarship where
  getModifiersFor (CorsairWarship a) =
    modified a a $ CannotHaveAttachments : [m | a.placement == StagingArea, m <- [CannotTakeDamage, GainKeyword (Archery 4)]]
