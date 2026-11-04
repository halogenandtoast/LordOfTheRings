module LordOfTheRings.Location.Cards.GateOfIron (gateOfIron) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype GateOfIron = GateOfIron LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gateOfIron :: LocationCard GateOfIron
gateOfIron = location GateOfIron Cards.gateOfIron

-- | While Gate of Iron is in the staging area, progress cannot be placed on locations in
-- the staging area, itself among them.
instance HasModifiersFor GateOfIron where
  getModifiersFor (GateOfIron a) = when (a.placement == StagingArea) do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressLimit 0]

-- | Travel: Discard the top card of the encounter deck. Then, the first player must discard
-- a card from his hand with cost greater than the discarded card's threat, or raise each
-- player's threat by 3. A card with no threat printed counts as 0.
instance RunMessage GateOfIron where
  runMessage msg l@(GateOfIron a) = case msg of
    TravelTo lid | lid == a.id -> do
      discardTopOfEncounterDeck a 1
      GateOfIron <$> runMessage msg a
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      let threat = sum [fromMaybe 0 (cdThreat $ toCardDef c) | c <- take 1 cards]
      lead <- getLead
      hand <- (.hand) <$> getPlayer lead
      chooseOrRunOneM lead do
        questionLabeled "gateOfIron.choose"
        causedBy a
        targets [c | c <- hand, printedCostOf c > threat] (discardFromHand lead)
        labeled "gateOfIron.raiseThreat" $ eachPlayer \p -> encounterRaisesThreat p 3
      pure l
    _ -> GateOfIron <$> runMessage msg a
