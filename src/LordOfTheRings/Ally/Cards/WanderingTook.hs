module LordOfTheRings.Ally.Cards.WanderingTook (wanderingTook) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype WanderingTook = WanderingTook AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wanderingTook :: AllyCard WanderingTook
wanderingTook = ally WanderingTook Cards.wanderingTook

-- | Action: Reduce your threat by 3 to give control of Wandering Took to another player.
-- Raise that player's threat by 3. (Limit once per round.)
instance HasAbilities WanderingTook where
  getAbilities (WanderingTook a) =
    [ limited (GroupLimit PerRound 1)
        $ restricted a 1 (PlayerExists $ NotPlayer controller) (ActionAbility Free)
    | Just pid <- [placementController a.placement]
    , let controller = PlayerWithId pid
    ]

instance RunMessage WanderingTook where
  runMessage msg s@(WanderingTook a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      others <- select (NotPlayer $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "wanderingTook.giveControl"
        causedBy a
        targets others \p -> do
          lowerThreat pid 3
          push $ TakeControl a.id p
          raiseThreat p 3
      pure s
    _ -> WanderingTook <$> runMessage msg a
