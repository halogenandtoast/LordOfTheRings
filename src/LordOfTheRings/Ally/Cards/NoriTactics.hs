module LordOfTheRings.Ally.Cards.NoriTactics (noriTactics) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype NoriTactics = NoriTactics AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

noriTactics :: AllyCard NoriTactics
noriTactics = ally NoriTactics Cards.noriTactics

-- | Response: After Nori participates in an attack that destroys an enemy, put the top card
-- of your discard pile on the bottom of your deck.
instance HasAbilities NoriTactics where
  getAbilities (NoriTactics a) =
    [ restricted a 1 (CardInDiscard pid AnyCard) $ response $ EnemyDestroyedByAttackWindow After (Just $ toTarget a)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage NoriTactics where
  runMessage msg s@(NoriTactics a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- headMay . (.discard) <$> getPlayer pid
      for_ top \c -> putDiscardOnBottomOfDeck pid c.id
      pure s
    _ -> NoriTactics <$> runMessage msg a
