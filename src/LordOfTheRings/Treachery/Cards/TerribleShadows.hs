module LordOfTheRings.Treachery.Cards.TerribleShadows (terribleShadows) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TerribleShadows = TerribleShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

terribleShadows :: TreacheryCard TerribleShadows
terribleShadows = treachery TerribleShadows Cards.terribleShadows

-- | When Revealed: Each character gets -1 willpower, -1 attack, and -1 defense until the
-- end of the round.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, raise the
-- defending player's threat by 3.
-- The lasting effect is on the characters in play as it resolves.
instance RunMessage TerribleShadows where
  runMessage msg t@(TerribleShadows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> roundModifiers attrs c [WillpowerModifier (-1), AttackModifier (-1), DefenseModifier (-1)]
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      encounterRaisesThreat pid 3
      pure t
    _ -> TerribleShadows <$> runMessage msg attrs
