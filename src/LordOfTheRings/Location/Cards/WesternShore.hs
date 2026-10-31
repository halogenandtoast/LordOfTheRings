module LordOfTheRings.Location.Cards.WesternShore (westernShore) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype WesternShore = WesternShore LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

westernShore :: LocationCard WesternShore
westernShore = location WesternShore Cards.westernShore

-- | X is the number of cards in the first player's hand. While Western Shore is in the
-- staging area, each enemy gets -X engagement cost.
instance HasModifiersFor WesternShore where
  getModifiersFor (WesternShore a) = do
    x <- length . (.hand) <$> (getPlayer =<< getLead)
    modified a a [ThreatModifier x]
    when (a.placement == StagingArea && x > 0) do
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [EngagementCostModifier (-x)]

-- | Shadow: If you have 5 or more cards in your hand, this attack is considered undefended.
instance RunMessage WesternShore where
  runMessage msg l@(WesternShore a) = case msg of
    ResolveShadowEffect cid eid pid (_ : _) | cid == a.cardId -> do
      n <- length . (.hand) <$> getPlayer pid
      when (n >= 5) $ makeUndefended pid eid
      pure l
    _ -> WesternShore <$> runMessage msg a
