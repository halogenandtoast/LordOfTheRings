module LordOfTheRings.Treachery.Cards.LotsOrNoneAtAll (lotsOrNoneAtAll) where

import LordOfTheRings.Card (CardDef (cdCardTraits), toCardDef)
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (trollInStaging, unlessBagginsCancel)
import LordOfTheRings.Trait (Trait (Troll))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LotsOrNoneAtAll = LotsOrNoneAtAll TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lotsOrNoneAtAll :: TreacheryCard LotsOrNoneAtAll
lotsOrNoneAtAll = treachery LotsOrNoneAtAll Cards.lotsOrNoneAtAll

-- | When Revealed: If there is a Troll enemy in the staging area, Sack 1. If there are no
-- Troll enemies in the staging area, this card gains Doomed 2.
-- Shadow: If the attacking enemy is a Troll, Sack 1. (The first player may spend 1 Baggins
-- resource to cancel this effect.)
instance RunMessage LotsOrNoneAtAll where
  runMessage msg t@(LotsOrNoneAtAll attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      troll <- trollInStaging
      if troll then sack 1 else raiseThreatFromDoomed 2
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      troll <- member Troll . cdCardTraits . toCardDef <$> getEnemy eid
      when troll $ unlessBagginsCancel attrs 1 (sack 1)
      pure t
    _ -> LotsOrNoneAtAll <$> runMessage msg attrs
