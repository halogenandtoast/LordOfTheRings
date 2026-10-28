module LordOfTheRings.Location.Cards.MarketSquare (marketSquare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype MarketSquare = MarketSquare LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

marketSquare :: LocationCard MarketSquare
marketSquare = location MarketSquare Cards.marketSquare

-- | Immune to player card effects.
-- Travel: Each player must spend 1 resource from one of his heroes' resource pools to
-- travel here.
instance HasModifiersFor MarketSquare where
  getModifiersFor (MarketSquare a) = do
    players <- getRemainingPlayers
    broke <- anyM (\p -> not <$> selectAny (HeroControlledBy (PlayerWithId p) <> HeroWithResources)) players
    modified a a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | broke]

-- | Shadow: Defending player discards all resources in his heroes' resource pools.
instance RunMessage MarketSquare where
  runMessage msg l@(MarketSquare a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer $ discardResourceFromAHero a
      MarketSquare <$> runMessage msg a
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
      for_ heroes \h -> when (h.resources > 0) $ removeTokens a h Resource h.resources
      pure l
    _ -> MarketSquare <$> runMessage msg a
