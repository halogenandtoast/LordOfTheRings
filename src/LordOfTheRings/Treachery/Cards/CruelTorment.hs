module LordOfTheRings.Treachery.Cards.CruelTorment (cruelTorment) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CruelTorment = CruelTorment TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cruelTorment :: TreacheryCard CruelTorment
cruelTorment = treachery CruelTorment Cards.cruelTorment

-- | Counts as a Condition attachment with the text: "Each player cannot have more than 6
-- cards in his hand. (Each player with more than 6 cards in his hand must immediately
-- discard cards until he has 6 cards in his hand.)"
instance HasModifiersFor CruelTorment where
  getModifiersFor (CruelTorment a) = case a.placement of
    AttachedTo _ -> traverse_ (\p -> modified a (PlayerTarget p) [HandSizeLimit 6]) =<< getRemainingPlayers
    _ -> pure ()

-- | Surge. When Revealed: Attach to the current quest.
instance RunMessage CruelTorment where
  runMessage msg t@(CruelTorment attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    _ -> CruelTorment <$> runMessage msg attrs
