module LordOfTheRings.Treachery.Cards.RecklessHate (recklessHate) where

import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RecklessHate = RecklessHate TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril, as its card def says.
recklessHate :: TreacheryCard RecklessHate
recklessHate = treachery RecklessHate Cards.recklessHate

{- | When Revealed: The enemy in the staging area with the highest attack makes an
immediate attack against you, of several tied the one you choose. If no attack is made
this way, Reckless Hate gains surge: with no enemy in the staging area.

Shadow: Attacking enemy makes an additional attack against you after this one.
-}
instance RunMessage RecklessHate where
  runMessage msg t@(RecklessHate attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      enemies <- traverse (\e -> (e,) <$> getEnemyAttack e) =<< select EnemyInStagingArea
      let highest = foldr (max . snd) 0 enemies
      if null enemies
        then revealEncounterCard
        else chooseOrRunOneM you do
          questionLabeled "recklessHate.choose"
          causedBy attrs
          targets [e | (e, n) <- enemies, n == highest] (immediateAttack you)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> RecklessHate <$> runMessage msg attrs
