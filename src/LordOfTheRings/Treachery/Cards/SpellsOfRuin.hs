module LordOfTheRings.Treachery.Cards.SpellsOfRuin (spellsOfRuin) where

import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (chooseDamageOrMinasTirith, damageMinasTirith)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SpellsOfRuin = SpellsOfRuin TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

spellsOfRuin :: TreacheryCard SpellsOfRuin
spellsOfRuin = treachery SpellsOfRuin Cards.spellsOfRuin

{- | When Revealed: Either deal 1 damage to each exhausted character, or deal 5 damage to
Minas Tirith. The first player chooses; at stage 2B, Minas Tirith not in play, the damage
goes to the exhausted characters (MEC54 rules, "Minas Tirith").
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, deal 2 damage
to Minas Tirith.
-}
instance RunMessage SpellsOfRuin where
  runMessage msg t@(SpellsOfRuin attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseDamageOrMinasTirith attrs lead "spellsOfRuin" 5 do
        exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
        for_ exhausted \c -> dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      damageMinasTirith attrs 2
      pure t
    _ -> SpellsOfRuin <$> runMessage msg attrs
