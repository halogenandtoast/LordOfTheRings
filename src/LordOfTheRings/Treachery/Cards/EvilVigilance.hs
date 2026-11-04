module LordOfTheRings.Treachery.Cards.EvilVigilance (evilVigilance) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EvilVigilance = EvilVigilance TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

evilVigilance :: TreacheryCard EvilVigilance
evilVigilance = treachery EvilVigilance Cards.evilVigilance

{- | When Revealed: Each player must choose: either discard a questing character you
control, or raise your threat by 1 for each questing character you control. A player with
no questing character has nothing to choose.
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, reveal the top
card of the encounter deck.
-}
instance RunMessage EvilVigilance where
  runMessage msg t@(EvilVigilance attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        questing <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> CommittedHero) (AllyControlledBy (PlayerWithId pid) <> CommittedAlly)
        unless (null questing) $ chooseOneM pid do
          questionLabeled "evilVigilance.choose"
          causedBy attrs
          targets questing discard
          labeledWith "evilVigilance.threat" [intVar "n" (length questing)] $ encounterRaisesThreat pid (length questing)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      revealEncounterCard
      pure t
    _ -> EvilVigilance <$> runMessage msg attrs
