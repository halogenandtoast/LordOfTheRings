module LordOfTheRings.Treachery.Cards.TurnedAround (turnedAround) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesEnemies, entitiesLocations, entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TurnedAround = TurnedAround TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

turnedAround :: TreacheryCard TurnedAround
turnedAround = treachery TurnedAround Cards.turnedAround

-- | When Revealed: Either remove 1 time counter from a card in play, or return the active
-- location to the staging area.
-- Shadow: If this attack is undefended, return the active location to the staging area.
instance RunMessage TurnedAround where
  runMessage msg t@(TurnedAround attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      es <- getsGame gameEntities
      let cards =
            map toTarget (toList $ entitiesQuests es)
              <> map toTarget (toList $ entitiesLocations es)
              <> map toTarget (toList $ entitiesEnemies es)
      timed <- filterM canRemoveTimeCounter cards
      active <- select IsActiveLocation
      lead <- getLead
      -- either one may be chosen, but one that can be done in full must be (FAQ 1.44)
      chooseOrRunOneM lead do
        questionLabeled "turnedAround.choose"
        causedBy attrs
        targets timed \c -> removeTimeCounters attrs c 1
        unless (null active) $ labeled "turnedAround.returnLocation" $ for_ active \l -> placeLocation l StagingArea
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      active <- select IsActiveLocation
      ifUndefended defenders (for_ active \l -> placeLocation l StagingArea) (pure ())
      pure t
    _ -> TurnedAround <$> runMessage msg attrs
