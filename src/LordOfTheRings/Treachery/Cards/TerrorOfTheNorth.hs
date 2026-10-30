module LordOfTheRings.Treachery.Cards.TerrorOfTheNorth (terrorOfTheNorth) where

import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (discardTopCards)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TerrorOfTheNorth = TerrorOfTheNorth TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

terrorOfTheNorth :: TreacheryCard TerrorOfTheNorth
terrorOfTheNorth = treachery TerrorOfTheNorth Cards.terrorOfTheNorth

-- | When Revealed: Each player discards the top 3 cards of his deck. Until the end of the
-- round, add 2 to the total threat in the staging area for each different card type
-- discarded this way: the types among all the cards the players discarded.
-- Shadow: Defending player discards the top 3 cards of his deck. Attacking enemy gets +1
-- attack for each different card type discarded this way.
instance RunMessage TerrorOfTheNorth where
  runMessage msg t@(TerrorOfTheNorth attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discarded <- concatMapM (`discardTopCards` 3) =<< getRemainingPlayers
      let types = length $ ordNub $ map (.cardType) discarded
      when (types > 0) $ roundModifier attrs GameTarget (StagingThreatBonus (2 * types))
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      types <- length . ordNub . map (.cardType) <$> discardTopCards pid 3
      when (types > 0) $ enemyGetsAttack attrs eid types
      pure t
    _ -> TerrorOfTheNorth <$> runMessage msg attrs
