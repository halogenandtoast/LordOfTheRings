module LordOfTheRings.Treachery.Cards.TheRammasIsBreached (theRammasIsBreached) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (chooseDamageOrMinasTirith)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheRammasIsBreached = TheRammasIsBreached TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRammasIsBreached :: TreacheryCard TheRammasIsBreached
theRammasIsBreached = treachery TheRammasIsBreached Cards.theRammasIsBreached

{- | When Revealed: Each player searches the encounter deck and discard pile for an enemy and
puts it into play engaged with him.
Shadow: Either deal 3 damage to Minas Tirith, or attacking enemy makes an additional attack
after this one. The defending player chooses.
-}
instance RunMessage TheRammasIsBreached where
  runMessage msg t@(TheRammasIsBreached attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> searchEncounterDeckAndDiscardToEngage p attrs (CardWithType EnemyType)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      chooseDamageOrMinasTirith attrs pid "theRammasIsBreached" 3 $ additionalAttack pid eid
      pure t
    _ -> TheRammasIsBreached <$> runMessage msg attrs
