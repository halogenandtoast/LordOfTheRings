module LordOfTheRings.Treachery.Cards.WeighedDownWilderland (weighedDownWilderland) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WeighedDownWilderland = WeighedDownWilderland TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Weighed Down, of the Wilderland set (The Hobbit: On the Doorstep).
weighedDownWilderland :: TreacheryCard WeighedDownWilderland
weighedDownWilderland = treachery WeighedDownWilderland Cards.weighedDownWilderland

-- | When Revealed: Until the end of the round, exhausted characters get -1 willpower, -1
-- attack, and -1 defense: those exhausted at any time this round, as they are.
-- Shadow: If the defending character is destroyed by this attack, its controller must raise
-- his threat by 5.
instance RunMessage WeighedDownWilderland where
  runMessage msg t@(WeighedDownWilderland attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      createEffect attrs GameTarget (map EachExhaustedCharacter [WillpowerModifier (-1), AttackModifier (-1), DefenseModifier (-1)]) UntilEndOfRound
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    -- the defending character destroyed, the player who controlled it raises his threat
    DoStep 1 (ResolveShadowEffect cid eid _ defenders) | cid == attrs.cardId -> do
      destroyed <- attackDestroyed eid
      for_ (nub [p | d <- destroyed, d.target `elem` defenders, Just p <- [d.controller]]) \p ->
        encounterRaisesThreat p 5
      pure t
    _ -> WeighedDownWilderland <$> runMessage msg attrs
