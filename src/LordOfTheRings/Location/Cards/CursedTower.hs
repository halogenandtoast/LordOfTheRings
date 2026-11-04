module LordOfTheRings.Location.Cards.CursedTower (cursedTower) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CursedTower = CursedTower LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedTower :: LocationCard CursedTower
cursedTower = location CursedTower Cards.cursedTower

-- | Discover 3. While Cursed Tower is in the staging area, it gains: "Forced: After a
-- player plays an event, raise his threat by 1."
instance HasAbilities CursedTower where
  getAbilities (CursedTower a) =
    [mkAbility a 1 $ forced $ CardPlayedWindow After (CardWithType EventType) | a.placement == StagingArea]

-- | Shadow: Discard a non-objective attachment you control.
-- An objective is not an attachment card here, so each of his attachments may go.
instance RunMessage CursedTower where
  runMessage msg l@(CursedTower a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [pid | Window After (PlayedCard pid _) <- ws] \pid -> encounterRaisesThreat pid 1
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard a pid attachments
      pure l
    _ -> CursedTower <$> runMessage msg a
