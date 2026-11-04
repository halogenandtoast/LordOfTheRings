module LordOfTheRings.Location.Cards.WatchedPath (watchedPath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheHuntForGollum.Helpers (playersWithClues)

newtype WatchedPath = WatchedPath LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

watchedPath :: LocationCard WatchedPath
watchedPath = location WatchedPath Cards.watchedPath

-- | Each player who does not control at least 1 Clue card cannot draw cards through card
-- effects.
instance HasModifiersFor WatchedPath where
  getModifiersFor (WatchedPath a) = do
    clued <- playersWithClues
    players <- getRemainingPlayers
    for_ (filter (`notElem` clued) players) \p -> modified a p [CannotDrawCardsFromPlayerCardEffects]

-- | Shadow: If you do not control at least 1 Clue card, discard all cards from your hand.
instance RunMessage WatchedPath where
  runMessage msg l@(WatchedPath a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      clued <- elem pid <$> playersWithClues
      unless clued $ discardHand pid
      pure l
    _ -> WatchedPath <$> runMessage msg a
