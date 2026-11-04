module LordOfTheRings.Ally.Cards.KnightOfBelfalas (knightOfBelfalas) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), CardType (AllyType))
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Gondor))

newtype KnightOfBelfalas = KnightOfBelfalas AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

knightOfBelfalas :: AllyCard KnightOfBelfalas
knightOfBelfalas = ally KnightOfBelfalas Cards.knightOfBelfalas

gondorAlly :: CardMatcher
gondorAlly = CardWithType AllyType <> CardWithTrait Gondor

-- | Response: After Knight of Belfalas enters play, return a Gondor ally from your discard
-- pile to your hand.
instance HasAbilities KnightOfBelfalas where
  getAbilities (KnightOfBelfalas a) =
    [restricted a 1 (CardInDiscard pid gondorAlly) $ response $ entersPlay a | Just pid <- [placementController a.placement]]

instance RunMessage KnightOfBelfalas where
  runMessage msg s@(KnightOfBelfalas a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      allies <- filter (cardMatches gondorAlly) . (.discard) <$> getPlayer pid
      chooseOrRunOneM pid do
        questionLabeled "knightOfBelfalas.return"
        causedBy a
        targets [pc | PlayerCard pc <- allies] \pc -> returnFromDiscardToHand pid pc.id
      pure s
    _ -> KnightOfBelfalas <$> runMessage msg a
