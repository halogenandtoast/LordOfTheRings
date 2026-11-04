module LordOfTheRings.Ally.Cards.Gamling (gamling) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), pcOwner)
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Rohan))

newtype Gamling = Gamling AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gamling :: AllyCard Gamling
gamling = ally Gamling Cards.gamling

-- | Response: After a Rohan ally you control is discarded from play, exhaust Gamling to
-- return that ally to your hand.
instance HasAbilities Gamling where
  getAbilities (Gamling a) =
    [ mkAbility a 1 $ ResponseAbility (AllyWithTraitDiscardedFromPlayWindow After pid Rohan) (exhaustCost a)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage Gamling where
  runMessage msg s@(Gamling a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [pc | Window _ (CharacterLeftPlay (PlayerCard pc)) <- windows]) \pc ->
        for_ (pcOwner pc) \owner -> returnFromDiscardToHand owner pc.id
      pure s
    _ -> Gamling <$> runMessage msg a
