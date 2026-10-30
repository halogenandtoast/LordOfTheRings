module LordOfTheRings.Location.Cards.NorburyTombs (norburyTombs) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (encounterDiscardPile)

newtype NorburyTombs = NorburyTombs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

norburyTombs :: LocationCard NorburyTombs
norburyTombs = location NorburyTombs Cards.norburyTombs

-- | While Norbury Tombs is the active location, it gains: "Response: When Norbury Tombs is
-- explored, each player shuffles the top 5 cards of his discard pile back into his deck."
instance HasAbilities NorburyTombs where
  getAbilities (NorburyTombs a) = [mkAbility a 1 $ response $ ExploredWindow When a.id | a.placement == ActiveLocation]

-- | Travel: Return the topmost enemy in the encounter discard pile to the staging area.
instance RunMessage NorburyTombs where
  runMessage msg l@(NorburyTombs a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> shuffleTopOfDiscardIntoDeck p 5
      pure l
    TravelTo lid | lid == a.id -> do
      discards <- encounterDiscardPile
      traverse_ addToStagingFromDiscard $ find ((== EnemyType) . cdCardType . toCardDef) discards
      pure l
    _ -> NorburyTombs <$> runMessage msg a
