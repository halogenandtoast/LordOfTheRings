module LordOfTheRings.Location.Cards.CapturedTower (capturedTower) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, InPlayArea))

newtype CapturedTower = CapturedTower LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

capturedTower :: LocationCard CapturedTower
capturedTower = location CapturedTower Cards.capturedTower

-- | While Captured Tower is the active location, encounter card effects cannot be canceled.
instance HasModifiersFor CapturedTower where
  getModifiersFor (CapturedTower a) = when (a.placement == ActiveLocation) $ modified a GameTarget [CannotCancelEncounterEffects]

-- | Forced: After a player takes control of Captured Tower, he shuffles the encounter
-- discard pile into the encounter deck and discards cards from the top until he discards an
-- enemy and puts that enemy into play engaged with him. The deck is read once shuffled.
instance RunMessage CapturedTower where
  runMessage msg l@(CapturedTower a) = case msg of
    PlaceLocation lid (InPlayArea pid) | lid == a.id && a.placement /= InPlayArea pid -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      CapturedTower <$> runMessage msg a
    DoStep 1 (PlaceLocation lid (InPlayArea pid)) | lid == a.id -> do
      discardEncounterCardsUntilFor a (PlayerTarget pid) enemyCard
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget pid) cards -> do
      for_ (find (cardMatches enemyCard) cards) \enemy -> do
        removeFromEncounterDiscard enemy
        putEnemyIntoPlayEngaged pid enemy
      pure l
    _ -> CapturedTower <$> runMessage msg a
   where
    enemyCard = CardWithType EnemyType
