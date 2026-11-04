module LordOfTheRings.Treachery.Cards.TheFiresSpread (theFiresSpread) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheFiresSpread = TheFiresSpread TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFiresSpread :: TreacheryCard TheFiresSpread
theFiresSpread = treachery TheFiresSpread Cards.theFiresSpread

-- | Doomed 2.
-- When Revealed: Deal 1 damage to each Aflame location in play. If less than 3 total
-- damage is dealt by this effect, The Fires Spread gains surge.
-- Shadow: If this attack destroys a character, deal 1 damage to each Aflame location in
-- play.
instance RunMessage TheFiresSpread where
  runMessage msg t@(TheFiresSpread attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      spread
      doStep 1 msg
      pure t
    -- the damage it dealt, none counted for a location that took none
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      dealt <- sum . map snd <$> damageDealtBy attrs
      when (dealt < 3) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      spread
      pure t
    _ -> TheFiresSpread <$> runMessage msg attrs
   where
    spread = do
      burning <- map fst <$> getAflameLocations
      for_ burning \l -> dealDamage attrs l 1
