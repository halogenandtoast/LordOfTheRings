module LordOfTheRings.Ally.Cards.Anborn (anborn) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Trap))

newtype Anborn = Anborn AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

anborn :: AllyCard Anborn
anborn = ally Anborn Cards.anborn

-- | Action: Exhaust Anborn to return 1 Trap card from your discard pile to your hand.
instance HasAbilities Anborn where
  getAbilities (Anborn a) =
    [ restricted a 1 (CardInDiscard pid $ CardWithTrait Trap) $ ActionAbility (exhaustCost a)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage Anborn where
  runMessage msg s@(Anborn a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      traps <- filter (cardMatches $ CardWithTrait Trap) . (.discard) <$> getPlayer pid
      chooseOrRunOneM pid do
        questionLabeled "anborn.chooseTrap"
        causedBy a
        targets [pc | PlayerCard pc <- traps] \pc -> returnFromDiscardToHand pid pc.id
      pure s
    _ -> Anborn <$> runMessage msg a
