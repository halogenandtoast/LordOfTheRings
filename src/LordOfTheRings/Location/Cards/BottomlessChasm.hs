module LordOfTheRings.Location.Cards.BottomlessChasm (bottomlessChasm) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Player.Types (discardableHand)

newtype BottomlessChasm = BottomlessChasm LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bottomlessChasm :: LocationCard BottomlessChasm
bottomlessChasm = location BottomlessChasm Cards.bottomlessChasm

-- | For each point of progress that is placed on Bottomless Chasm, the players must choose
-- and discard 1 card from a player's hand, if able: the first player chooses the player.
-- Shadow: If this attack destroys a character, the defending player discards all cards in
-- his hand.
instance RunMessage BottomlessChasm where
  runMessage msg l@(BottomlessChasm a) = case msg of
    PlaceTokens _ (isTarget a -> True) Progress n | n > 0 -> do
      replicateM_ n $ doStep 1 msg
      BottomlessChasm <$> runMessage msg a
    DoStep 1 (PlaceTokens _ (isTarget a -> True) Progress _) -> do
      lead <- getLead
      holding <- filterM (fmap (notNull . discardableHand) . getPlayer) =<< getRemainingPlayers
      chooseOrRunOneM lead do
        questionLabeled "bottomlessChasm.discard"
        causedBy a
        targets holding \p -> chooseAndDiscardFromHand p a 1
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == a.cardId -> do
      discardHand pid
      pure l
    _ -> BottomlessChasm <$> runMessage msg a
