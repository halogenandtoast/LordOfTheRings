module LordOfTheRings.Ally.Cards.WanderingExile (wanderingExile) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype WanderingExile = WanderingExile AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wanderingExile :: AllyCard WanderingExile
wanderingExile = ally WanderingExile Cards.wanderingExile

-- | Response: After Wandering Exile commits to a quest, discard a card from your hand to
-- ready Wandering Exile.
instance HasAbilities WanderingExile where
  getAbilities (WanderingExile a) =
    [ mkAbility a 1 $ ResponseAbility (CommittedToQuestWindow After (toTarget a)) (DiscardFromHandCost pid 1)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage WanderingExile where
  runMessage msg s@(WanderingExile a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> WanderingExile <$> runMessage msg a
