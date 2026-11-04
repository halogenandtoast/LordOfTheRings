module LordOfTheRings.Location.Cards.StonewainValley (stonewainValley) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype StonewainValley = StonewainValley LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

stonewainValley :: LocationCard StonewainValley
stonewainValley = location StonewainValley Cards.stonewainValley

-- | Stonewain Valley gets +1 threat for each resource on it.
instance HasModifiersFor StonewainValley where
  getModifiersFor (StonewainValley a) = do
    let n = countTokens Resource a.locationTokens
    when (n > 0) $ modified a a [ThreatModifier n]

-- | While Stonewain Valley is in the staging area, resources discarded or spent by the
-- first player for any reason are placed on Stonewain Valley: each one taken from a pool
-- of his heroes.
instance RunMessage StonewainValley where
  runMessage msg (StonewainValley a) = case msg of
    RemoveTokens _ (HeroTarget hid) Resource n | n > 0 && a.placement == StagingArea -> do
      lead <- getLead
      owner <- (.owner) <$> getHero hid
      when (owner == lead) $ placeResources a a n
      StonewainValley <$> runMessage msg a
    _ -> StonewainValley <$> runMessage msg a
