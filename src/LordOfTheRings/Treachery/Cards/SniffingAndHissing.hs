module LordOfTheRings.Treachery.Cards.SniffingAndHissing (sniffingAndHissing) where

import LordOfTheRings.Helpers.Fellowship (getReadySagaOneRing)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SniffingAndHissing = SniffingAndHissing TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sniffingAndHissing :: TreacheryCard SniffingAndHissing
sniffingAndHissing = treachery SniffingAndHissing Cards.sniffingAndHissing

instance RunMessage SniffingAndHissing where
  runMessage msg t@(SniffingAndHissing a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      ring <- getReadySagaOneRing
      chooseOrRunOneM lead do
        questionLabeled "sniffingAndHissing.choose"
        causedBy a
        for_ ring \oid -> labeled "sniffingAndHissing.exhaust" do
          exhaust (ObjectiveTarget oid)
          revealEncounterCard
        labeled "sniffingAndHissing.hide" $ hideTest lead a 3
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 2
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == a.cardId -> do
      encounterRaisesThreat pid 3
      pure t
    _ -> SniffingAndHissing <$> runMessage msg a
