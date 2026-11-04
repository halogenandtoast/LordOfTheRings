module LordOfTheRings.Treachery.Cards.MarchingUpTheRoad (marchingUpTheRoad) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (placeUnderTheBlackGate)
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (searchDeckAndDiscardFor)
import LordOfTheRings.Trait (Trait (Harad))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MarchingUpTheRoad = MarchingUpTheRoad TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

marchingUpTheRoad :: TreacheryCard MarchingUpTheRoad
marchingUpTheRoad = treachery MarchingUpTheRoad Cards.marchingUpTheRoad

haradEnemyCard :: CardMatcher
haradEnemyCard = CardMatchAll [CardWithType EnemyType, CardWithTrait Harad]

{- | When Revealed: Each player must either return an enemy engaged with him to the staging
area, or search the encounter deck and discard pile for a Harad enemy and put it into play
engaged with him. Only an option that can be done in full is offered (FAQ 1.44). One player
after another, each choosing as the one before left the game.
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, place
attacking enemy facedown under The Black Gate.
-}
instance RunMessage MarchingUpTheRoad where
  runMessage msg t@(MarchingUpTheRoad attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure t
    DoStep n inner@(WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (headMay $ drop (n - 1) players) \pid -> do
        engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
        deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
        discardPile <- getEncounterDiscard
        let found = any (cardMatches haradEnemyCard) (deck <> discardPile)
        chooseOrRunOneM pid do
          questionLabeled "marchingUpTheRoad.choose"
          causedBy attrs
          unless (null engaged) $ labeled "marchingUpTheRoad.return" do
            chooseOrRunOneM pid do
              questionLabeled "marchingUpTheRoad.enemy"
              causedBy attrs
              targets engaged \e -> placeEnemy e StagingArea
          when found
            $ labeled "marchingUpTheRoad.search"
            $ searchDeckAndDiscardFor pid "marchingUpTheRoad.searchFor" haradEnemyCard (putEnemyIntoPlayEngaged pid)
        doStep (n + 1) inner
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      placeUnderTheBlackGate eid
      pure t
    _ -> MarchingUpTheRoad <$> runMessage msg attrs
