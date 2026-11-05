module LordOfTheRings.Ally.Cards.HonourGuard (honourGuard) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype HonourGuard = HonourGuard AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

honourGuard :: AllyCard HonourGuard
honourGuard = ally HonourGuard Cards.honourGuard

-- | Response: Exhaust Honour Guard to cancel 1 point of damage just dealt to a character.
-- Valour Response: Exhaust and discard Honour Guard to cancel up to 5 damage just dealt to
-- a character.
instance HasAbilities HonourGuard where
  getAbilities (HonourGuard a) =
    mkAbility a 1 (ResponseAbility (CharacterDealtDamageWindow After) (exhaustCost a))
      : [ restricted a 2 (valour pid) $ ResponseAbility (CharacterDealtDamageWindow After) (exhaustCost a)
        | Just pid <- [placementController a.placement]
        ]

-- | The damage is on the character, its destruction not yet checked: the canceled points
-- come off before it is, as Gondorian Discipline's.
instance RunMessage HonourGuard where
  runMessage msg s@(HonourGuard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [t | Window _ (DealtDamage t _) <- windows]) \t -> removeTokens a t Damage 1
      pure s
    UseCardAbility _ (isSource a -> True) 2 windows -> do
      discard a a
      for_ (take 1 [(t, n) | Window _ (DealtDamage t n) <- windows]) \(t, n) -> removeTokens a t Damage (min 5 n)
      pure s
    _ -> HonourGuard <$> runMessage msg a
