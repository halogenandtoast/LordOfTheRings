module LordOfTheRings.Location.Cards.HilltopBattlements (hilltopBattlements) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype HilltopBattlements = HilltopBattlements LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

hilltopBattlements :: LocationCard HilltopBattlements
hilltopBattlements = location HilltopBattlements Cards.hilltopBattlements

-- | While Hilltop Battlements is in the staging area, each non-unique enemy gets -5
-- engagement cost.
instance HasModifiersFor HilltopBattlements where
  getModifiersFor (HilltopBattlements a) = when (a.placement == StagingArea) do
    enemies <- select (not_ UniqueEnemy)
    for_ enemies \e -> modified a e [EngagementCostModifier (-5)]

-- | Forced: When Hilltop Battlements is explored, discard the top X cards of the encounter
-- deck, where X is the number of players in the game. Add each enemy discarded by this
-- effect to the staging area.
instance HasAbilities HilltopBattlements where
  getAbilities (HilltopBattlements a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage HilltopBattlements where
  runMessage msg l@(HilltopBattlements a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      players <- length <$> getRemainingPlayers
      discardTopOfEncounterDeck a players
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType EnemyType) cards) addToStagingFromDiscard
      pure l
    _ -> HilltopBattlements <$> runMessage msg a
