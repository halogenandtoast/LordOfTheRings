module LordOfTheRings.Treachery.Cards.FallenStones (fallenStones) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FallenStones = FallenStones TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fallenStones :: TreacheryCard FallenStones
fallenStones = treachery FallenStones Cards.fallenStones

-- | When Revealed: The first player (choose 1): removes all progress tokens from play, or
-- reveals 2 cards from the encounter deck and adds them to the staging area.
-- Shadow: attacking enemy gets +1 [Attack] for each progress token on the active location.
instance RunMessage FallenStones where
  runMessage msg t@(FallenStones attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "fallenStones.chooseOne"
        causedBy attrs
        labeled "fallenStones.removeProgress" $ removeAllProgress attrs
        labeled "fallenStones.reveal" $ revealEncounterCards 2
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      progress <- activeLocationProgress
      when (progress > 0) $ enemyGetsAttack attrs eid progress
      pure t
    _ -> FallenStones <$> runMessage msg attrs
