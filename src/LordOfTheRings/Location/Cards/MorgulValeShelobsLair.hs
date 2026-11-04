module LordOfTheRings.Location.Cards.MorgulValeShelobsLair (morgulValeShelobsLair) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (discardRandomFromEachHand)
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (searchDeckAndDiscardFor)

newtype MorgulValeShelobsLair = MorgulValeShelobsLair LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Shelob's Lair's Morgul Vale, made the active location by stage 1A.
morgulValeShelobsLair :: LocationCard MorgulValeShelobsLair
morgulValeShelobsLair = location MorgulValeShelobsLair Cards.morgulValeShelobsLair

-- | Immune to player card effects.
instance HasModifiersFor MorgulValeShelobsLair where
  getModifiersFor (MorgulValeShelobsLair a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After Morgul Vale is explored, search the encounter deck and discard pile for a
-- Morgul Wraith and add it to the staging area. Shuffle the encounter deck. Then, the first
-- player must either exhaust The One Ring, or discard 1 random card from each player's
-- hand.
instance HasAbilities MorgulValeShelobsLair where
  getAbilities (MorgulValeShelobsLair a) = [mkAbility a 1 $ forced $ ExploredWindow After a.id]

-- | The first player searches and chooses; with The One Ring exhausted, or out of play, the
-- cards are discarded.
instance RunMessage MorgulValeShelobsLair where
  runMessage msg l@(MorgulValeShelobsLair a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      searchDeckAndDiscardFor lead "morgulVale.search" (cardIs Enemies.morgulWraith) addToStaging
      ring <- getSagaOneRing
      chooseOrRunOneM lead do
        questionLabeled "morgulVale.choose"
        causedBy a
        for_ (filter (not . (.exhausted)) (toList ring)) \r -> labeled "morgulVale.exhaustTheOneRing" $ exhaust (toTarget r)
        labeled "morgulVale.discard" $ discardRandomFromEachHand 1
      pure l
    _ -> MorgulValeShelobsLair <$> runMessage msg a
