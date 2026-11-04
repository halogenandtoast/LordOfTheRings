module LordOfTheRings.Location.Cards.MoonlitPassage (moonlitPassage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype MoonlitPassage = MoonlitPassage LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

moonlitPassage :: LocationCard MoonlitPassage
moonlitPassage = location MoonlitPassage Cards.moonlitPassage

-- | Discover 2. While Moonlit Passage is in the staging area, each player's threat cannot
-- be reduced by more than 1 via player card effects each round.
-- What player cards reduced it this round counts, from before it arrived too.
instance HasModifiersFor MoonlitPassage where
  getModifiersFor (MoonlitPassage a) = when (a.placement == StagingArea) do
    players <- getRemainingPlayers
    for_ players \p -> do
      reduced <- getThreatReducedByPlayerCardsThisRound p
      modified a (PlayerTarget p) [PlayerCardThreatReductionLimit (max 0 $ 1 - reduced)]

-- | Shadow: Raise your threat by 1.
instance RunMessage MoonlitPassage where
  runMessage msg l@(MoonlitPassage a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      encounterRaisesThreat pid 1
      pure l
    _ -> MoonlitPassage <$> runMessage msg a
