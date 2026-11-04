module LordOfTheRings.Treachery.Cards.BurningReek (burningReek) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BurningReek = BurningReek TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 1, as its card def says.
burningReek :: TreacheryCard BurningReek
burningReek = treachery BurningReek Cards.burningReek

-- | When Revealed: Each player discards 1 card from the top of his deck for each character
-- he controls.
-- Shadow: Either discard the top 2 cards of your deck, or return attacking enemy to the
-- staging area after this attack. The defending player chooses; the discard is offered
-- only with 2 cards to discard (FAQ 1.44).
instance RunMessage BurningReek where
  runMessage msg t@(BurningReek attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        characters <- selectCharacters (HeroControlledBy $ PlayerWithId p) (AllyControlledBy $ PlayerWithId p)
        discardTopOfDeck p (length characters)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      deck <- (.deck) <$> getPlayer pid
      chooseOrRunOneM pid do
        questionLabeled "burningReek.choose"
        causedBy attrs
        when (length deck >= 2) $ labeled "burningReek.discard" $ discardTopOfDeck pid 2
        labeled "burningReek.return" $ doStep 1 msg
      pure t
    -- the attack still to resolve is found once the choice is made
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> BurningReek <$> runMessage msg attrs
