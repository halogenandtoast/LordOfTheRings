module LordOfTheRings.Treachery.Cards.ManyThongedWhip (manyThongedWhip) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ManyThongedWhip = ManyThongedWhip TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

manyThongedWhip :: TreacheryCard ManyThongedWhip
manyThongedWhip = treachery ManyThongedWhip Cards.manyThongedWhip

-- | Counts as a Weapon attachment with the text: "Forced: When Durin's Bane attacks, the
-- defending player must discard 1 card at random from his hand."
instance HasAbilities ManyThongedWhip where
  getAbilities (ManyThongedWhip a) = case a.placement of
    AttachedTo (EnemyTarget eid) -> [mkAbility a 1 $ forced $ EnemyAttacksWindow When eid]
    _ -> []

-- | When Revealed: Attach Many Thonged Whip to Durin's Bane.
instance RunMessage ManyThongedWhip where
  runMessage msg t@(ManyThongedWhip attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      attachToDurinsBane attrs
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [p | Window _ (EnemyAttacking _ p) <- ws] (`discardRandomFromHand` 1)
      pure t
    _ -> ManyThongedWhip <$> runMessage msg attrs
