module LordOfTheRings.Location.Cards.OppressiveForest (oppressiveForest) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Helpers.Modifiers (cardsPlayedThisRound)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype OppressiveForest = OppressiveForest LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

oppressiveForest :: LocationCard OppressiveForest
oppressiveForest = location OppressiveForest Cards.oppressiveForest

instance HasModifiersFor OppressiveForest where
  getModifiersFor (OppressiveForest a) = when (a.placement == StagingArea) do
    played <- map (\(pid, _, _) -> pid) <$> cardsPlayedThisRound
    for_ (nub played) \pid -> modified a (PlayerTarget pid) [CannotPlay AnyCard]

instance RunMessage OppressiveForest where
  runMessage msg l@(OppressiveForest a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      unlessM (selectAny LocationInStagingArea) $ addShadowCardToStaging eid (MkEncounterCard cid (toCardCode a))
      pure l
    _ -> OppressiveForest <$> runMessage msg a
