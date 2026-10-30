module LordOfTheRings.Ally.Cards.MithlondSeaWatcher (mithlondSeaWatcher) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Placement (placementController)

newtype MithlondSeaWatcher = MithlondSeaWatcher AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

mithlondSeaWatcher :: AllyCard MithlondSeaWatcher
mithlondSeaWatcher = ally MithlondSeaWatcher Cards.mithlondSeaWatcher

-- | While the top card of your discard pile is an ally, Mithlond Sea-watcher gets +2
-- attack and gains ranged.
instance HasModifiersFor MithlondSeaWatcher where
  getModifiersFor (MithlondSeaWatcher a) = for_ (placementController a.placement) \pid ->
    whenM (topOfDiscardIs AllyType pid) $ modifySelf a [AttackModifier 2, GainKeyword Ranged]
