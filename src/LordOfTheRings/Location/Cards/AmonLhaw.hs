module LordOfTheRings.Location.Cards.AmonLhaw (amonLhaw) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype AmonLhaw = AmonLhaw LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

amonLhaw :: LocationCard AmonLhaw
amonLhaw = location AmonLhaw Cards.amonLhaw

-- | X is double the number of players in the game. While Amon Lhaw is the active location,
-- treat all attachments as if their printed text boxes were blank.
instance HasModifiersFor AmonLhaw where
  getModifiersFor (AmonLhaw a) = do
    players <- length <$> getRemainingPlayers
    modified a a [ThreatModifier (2 * players)]
    when (a.placement == ActiveLocation) $ modified a GameTarget [BlankAttachments]
