module LordOfTheRings.Location.Cards.CityOfCorsairs (cityOfCorsairs) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Corsair, Raider))

newtype CityOfCorsairs = CityOfCorsairs LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cityOfCorsairs :: LocationCard CityOfCorsairs
cityOfCorsairs = location CityOfCorsairs Cards.cityOfCorsairs

-- | X is the highest number of resources on a Raider enemy in play.
instance HasModifiersFor CityOfCorsairs where
  getModifiersFor (CityOfCorsairs a) = do
    raiders <- traverse getEnemy =<< select (EnemyWithTrait Raider)
    let x = fromMaybe 0 $ maximumMay $ map (countTokens Resource . enemyTokens . toAttrs) raiders
    when (x > 0) $ modifySelf a [ThreatModifier x]

-- | Travel: Search the encounter deck and discard pile for a Corsair enemy and add it to the
-- staging area. Shuffle the encounter deck.
instance RunMessage CityOfCorsairs where
  runMessage msg (CityOfCorsairs a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead a (CardWithType EnemyType <> CardWithTrait Corsair)
      CityOfCorsairs <$> runMessage msg a
    _ -> CityOfCorsairs <$> runMessage msg a
