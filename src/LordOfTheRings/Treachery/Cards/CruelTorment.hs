module LordOfTheRings.Treachery.Cards.CruelTorment (cruelTorment) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CruelTorment = CruelTorment TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cruelTorment :: TreacheryCard CruelTorment
cruelTorment = treachery CruelTorment Cards.cruelTorment

-- | Surge.
-- When Revealed: Attach to the current quest. Counts as a Condition attachment with the
-- text: "Each player cannot have more than 6 cards in his hand. (Each player with more than
-- 6 cards in his hand must immediately discard cards until he has 6 cards in his hand.)"
instance RunMessage CruelTorment where
  runMessage msg t@(CruelTorment attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      eachPlayer trimHand
      pure t
    CheckWindows ws | AttachedTo _ <- attrs.placement -> do
      for_ (ordNub [pid | Window After (CardsDrawn pid _) <- ws]) trimHand
      CruelTorment <$> runMessage msg attrs
    _ -> CruelTorment <$> runMessage msg attrs
   where
    trimHand pid = do
      n <- length . (.hand) <$> getPlayer pid
      when (n > 6) $ chooseAndDiscardFromHand pid attrs (n - 6)
