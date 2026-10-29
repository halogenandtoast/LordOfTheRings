module LordOfTheRings.Treachery.Cards.RemnantsOfElderDays (remnantsOfElderDays) where

import LordOfTheRings.Card (CardType (EnemyType), Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Trait (Trait (Creature))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RemnantsOfElderDays = RemnantsOfElderDays TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

remnantsOfElderDays :: TreacheryCard RemnantsOfElderDays
remnantsOfElderDays = treachery RemnantsOfElderDays Cards.remnantsOfElderDays

-- | This effect cannot be canceled.
instance HasModifiersFor RemnantsOfElderDays where
  getModifiersFor (RemnantsOfElderDays a) = modified a a [WhenRevealedCannotBeCanceled]

-- | When Revealed: Each player must search the encounter deck or discard pile for a
-- Creature enemy and put it into play engaged with him. Shuffle the encounter deck.
--
-- Each player searches in turn, from the first player, among what the players before him
-- left.
instance RunMessage RemnantsOfElderDays where
  runMessage msg t@(RemnantsOfElderDays attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure t
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          scenario <- getsGame (toAttrs . (.scenario))
          let creature = cardMatches (CardWithType EnemyType <> CardWithTrait Creature)
              fromDeck = filter creature scenario.encounterDeck
              fromDiscard = filter creature scenario.encounterDiscard
          chooseOneM pid do
            questionLabeled "remnantsOfElderDays.search"
            causedBy attrs
            showing (map EncounterCard $ fromDeck <> fromDiscard)
            targets fromDeck \c -> do
              removeFromEncounterDeck c
              putEnemyIntoPlayEngaged pid c
            targets fromDiscard \c -> do
              removeFromEncounterDiscard c
              putEnemyIntoPlayEngaged pid c
          doStep (n + 1) (WhenRevealed $ toTarget attrs)
        [] -> shuffleEncounterDeck
      pure t
    _ -> RemnantsOfElderDays <$> runMessage msg attrs
