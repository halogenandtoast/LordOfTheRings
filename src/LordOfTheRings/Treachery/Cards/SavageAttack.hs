module LordOfTheRings.Treachery.Cards.SavageAttack (savageAttack) where

import LordOfTheRings.Card (CardCost (StaticCost), cdCost, toCardDef)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SavageAttack = SavageAttack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

savageAttack :: TreacheryCard SavageAttack
savageAttack = treachery SavageAttack Cards.savageAttack

-- | When Revealed: Each player must choose: either discard the highest cost ally you
-- control, or reveal an encounter card.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, reveal the
-- top card of the encounter deck.
-- Of allies tied for the highest cost, the player discards the one he chooses; one with no
-- printed cost counts as 0, as Predatory Wolves reads it.
instance RunMessage SavageAttack where
  runMessage msg t@(SavageAttack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        allies <- select (AllyControlledBy $ PlayerWithId p)
        costs <- for allies \aid -> (aid,) . printedCost . toCardDef <$> getAlly aid
        let highest = maximumMay (map snd costs)
        chooseOrRunOneM p do
          questionLabeled "savageAttack.choose"
          causedBy attrs
          unless (null allies) $ labeled "savageAttack.discardAlly" do
            chooseOrRunOneM p do
              questionLabeled "savageAttack.chooseAlly"
              causedBy attrs
              targets [aid | (aid, c) <- costs, Just c == highest] (discard attrs)
          labeled "savageAttack.reveal" revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      revealEncounterCard
      pure t
    _ -> SavageAttack <$> runMessage msg attrs
   where
    printedCost def = case cdCost def of
      Just (StaticCost x) -> x
      _ -> 0
