module LordOfTheRings.Ally.Cards.EreborHammersmith (ereborHammersmith) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card, CardType (AttachmentType))

newtype EreborHammersmith = EreborHammersmith AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ereborHammersmith :: AllyCard EreborHammersmith
ereborHammersmith = ally EreborHammersmith Cards.ereborHammersmith

-- | Response: After you play Erebor Hammersmith, return the topmost attachment in any
-- player's discard pile to his hand.
instance HasAbilities EreborHammersmith where
  getAbilities (EreborHammersmith a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage EreborHammersmith where
  runMessage msg s@(EreborHammersmith a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      order <- getPlayerOrder
      tops <- forMaybeM order \p -> fmap (p,) . topAttachment . (.discard) <$> getPlayer p
      unless (null tops)
        $ push
        $ chooseOrRunOne pid [targetLabel (CardIdTarget c.id) [ReturnFromDiscardToHand p c.id] | (p, c) <- tops]
      pure s
    _ -> EreborHammersmith <$> runMessage msg a
   where
    -- a discard pile is kept with the most recently discarded card first
    topAttachment :: [Card] -> Maybe Card
    topAttachment = find ((== AttachmentType) . (.cardType))
