module LordOfTheRings.Treachery.Cards.FellFolkOfUmbar (fellFolkOfUmbar) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (corsairEnemy)
import LordOfTheRings.Trait (Trait (Corsair))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FellFolkOfUmbar = FellFolkOfUmbar TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
fellFolkOfUmbar :: TreacheryCard FellFolkOfUmbar
fellFolkOfUmbar = treachery FellFolkOfUmbar Cards.fellFolkOfUmbar

-- | When Revealed: Either return each Corsair enemy engaged with you to the staging area,
-- or discard cards from the top of the encounter deck until a Corsair enemy is discarded
-- and put that enemy into play engaged with you. The first is offered only with a Corsair
-- engaged with you to return.
instance RunMessage FellFolkOfUmbar where
  runMessage msg t@(FellFolkOfUmbar attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealer <- getRevealer
      corsairs <- select (EnemyWithTrait Corsair <> EnemyEngagedWith (PlayerWithId revealer))
      chooseOrRunOneM revealer do
        questionLabeled "fellFolkOfUmbar.choose"
        causedBy attrs
        unless (null corsairs) $ labeled "fellFolkOfUmbar.return" $ for_ corsairs (`placeEnemy` StagingArea)
        labeled "fellFolkOfUmbar.discard" $ discardEncounterCardsUntilFor attrs (PlayerTarget revealer) corsairEnemy
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget revealer) cards -> do
      for_ (find (cardMatches corsairEnemy) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged revealer card
      pure t
    _ -> FellFolkOfUmbar <$> runMessage msg attrs
