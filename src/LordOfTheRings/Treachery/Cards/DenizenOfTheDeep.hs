module LordOfTheRings.Treachery.Cards.DenizenOfTheDeep (denizenOfTheDeep) where

import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DenizenOfTheDeep = DenizenOfTheDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

denizenOfTheDeep :: TreacheryCard DenizenOfTheDeep
denizenOfTheDeep = treachery DenizenOfTheDeep Cards.denizenOfTheDeep

{- | When Revealed: The highest attack enemy in the staging area makes an immediate attack
against the first player. If no attack is made this way, Denizen of the Deep gains surge.
The first player chooses among enemies tied for the highest attack.
Shadow: Attacking enemy makes an additional attack against you after this one.
-}
instance RunMessage DenizenOfTheDeep where
  runMessage msg t@(DenizenOfTheDeep attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      staged <- traverse (\e -> (e,) <$> getEnemyAttack e) =<< select EnemyInStagingArea
      let highest = maximumMay (map snd staged)
      lead <- getLead
      if null staged
        then revealEncounterCard
        else chooseOrRunOneM lead do
          questionLabeled "denizenOfTheDeep.choose"
          causedBy attrs
          targets [e | (e, n) <- staged, Just n == highest] (immediateAttack lead)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> DenizenOfTheDeep <$> runMessage msg attrs
