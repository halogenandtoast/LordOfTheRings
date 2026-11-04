module LordOfTheRings.Treachery.Cards.CaughtOffGuard (caughtOffGuard) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Helpers.ControlledLocation (controlledLocations, returnToStaging)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CaughtOffGuard = CaughtOffGuard TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

caughtOffGuard :: TreacheryCard CaughtOffGuard
caughtOffGuard = treachery CaughtOffGuard Cards.caughtOffGuard

{- | When Revealed: Each player must choose: either return a location you control to the
staging area, or discard cards from the top of the encounter deck until an enemy is
discarded and put it into play engaged with you. In turn order, each player's choice made
once the last one's has resolved; a player controlling no location discards.
Shadow: Attacking enemy makes an additional attack against you after this one.
-}
instance RunMessage CaughtOffGuard where
  runMessage msg t@(CaughtOffGuard attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure t
    DoStep n inner@(WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (take 1 $ drop (n - 1) players) \pid -> do
        locations <- controlledLocations pid
        chooseOrRunOneM pid do
          questionLabeled "caughtOffGuard.choose"
          causedBy attrs
          targets locations returnToStaging
          labeled "caughtOffGuard.discard" $ discardEncounterCardsUntilFor attrs (PlayerTarget pid) enemyCard
        doStep (n + 1) inner
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget pid) cards -> do
      for_ (find (cardMatches enemyCard) cards) \enemy -> do
        removeFromEncounterDiscard enemy
        putEnemyIntoPlayEngaged pid enemy
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> CaughtOffGuard <$> runMessage msg attrs
   where
    enemyCard = CardWithType EnemyType
