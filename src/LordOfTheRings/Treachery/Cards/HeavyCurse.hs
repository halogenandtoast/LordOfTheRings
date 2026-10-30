module LordOfTheRings.Treachery.Cards.HeavyCurse (heavyCurse) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (cardTitle)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HeavyCurse = HeavyCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

heavyCurse :: TreacheryCard HeavyCurse
heavyCurse = treachery HeavyCurse Cards.heavyCurse

-- | Counts as a Condition attachment with the text: "When a player plays a card, increase
-- its cost by 1 if there is a card that has the same title in its owner's discard pile."
-- Each player's cards of a title in his discard pile cost him 1 more.
instance HasModifiersFor HeavyCurse where
  getModifiersFor (HeavyCurse a) = case a.placement of
    AttachedTo _ -> do
      players <- traverse getPlayer =<< getRemainingPlayers
      for_ players \p -> do
        let named = ordNub $ map (cardTitle . toCardDef) p.discard
        unless (null named) $ modified a (PlayerTarget p.id) [IncreaseCost (CardMatchAny $ map CardWithTitle named) 1]
    _ -> pure ()

-- | Surge.
-- When Revealed: Attach to the current quest.
instance RunMessage HeavyCurse where
  runMessage msg t@(HeavyCurse attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    _ -> HeavyCurse <$> runMessage msg attrs
