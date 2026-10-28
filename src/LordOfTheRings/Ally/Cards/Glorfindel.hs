module LordOfTheRings.Ally.Cards.Glorfindel (glorfindel) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype Glorfindel = Glorfindel AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Glorfindel can be played from your discard pile, as his card def says.
glorfindel :: AllyCard Glorfindel
glorfindel = ally Glorfindel Cards.glorfindel

-- | Action: Discard a card from your hand to ready Glorfindel. (Limit once per phase.)
instance HasAbilities Glorfindel where
  getAbilities (Glorfindel a) =
    [ limited (GroupLimit PerPhase 1) $ mkAbility a 1 $ ActionAbility (DiscardFromHandCost pid 1)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage Glorfindel where
  runMessage msg s@(Glorfindel a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> Glorfindel <$> runMessage msg a
