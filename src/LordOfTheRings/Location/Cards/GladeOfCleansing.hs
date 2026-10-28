module LordOfTheRings.Location.Cards.GladeOfCleansing (gladeOfCleansing) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Wose))

newtype GladeOfCleansing = GladeOfCleansing LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gladeOfCleansing :: LocationCard GladeOfCleansing
gladeOfCleansing = location GladeOfCleansing Cards.gladeOfCleansing

-- | Each Wose enemy gains archery 1.
-- X is equal to the total archery value of the highest archery Wose enemy in play.
instance HasModifiersFor GladeOfCleansing where
  getModifiersFor (GladeOfCleansing a) = do
    woses <- select (EnemyWithTrait Wose)
    for_ woses \w -> modified a w [GainKeyword (Archery 1)]
    -- the archery each has, the 1 this gives among it
    x <- foldr max 0 <$> for woses \w -> getArchery (toTarget w) . toCardDef =<< getEnemy w
    when (x > 0) $ modified a a [ThreatModifier x]

instance RunMessage GladeOfCleansing where
  runMessage msg (GladeOfCleansing a) = GladeOfCleansing <$> runMessage msg a
