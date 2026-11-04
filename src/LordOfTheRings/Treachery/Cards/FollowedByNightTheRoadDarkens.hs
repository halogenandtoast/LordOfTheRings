module LordOfTheRings.Treachery.Cards.FollowedByNightTheRoadDarkens (followedByNightTheRoadDarkens) where

import LordOfTheRings.Card (CardDef (cdCardType), CardType (EnemyType), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Scenario.Types (scenarioEncounterDiscard)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FollowedByNightTheRoadDarkens = FollowedByNightTheRoadDarkens TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of Breaking of the Fellowship's burden set.
followedByNightTheRoadDarkens :: TreacheryCard FollowedByNightTheRoadDarkens
followedByNightTheRoadDarkens = treachery FollowedByNightTheRoadDarkens Cards.followedByNightTheRoadDarkens

-- | Peril. Surge. When Revealed: Put the topmost enemy of the encounter deck discard pile
-- into play engaged with you.
instance RunMessage FollowedByNightTheRoadDarkens where
  runMessage msg t@(FollowedByNightTheRoadDarkens attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      discarded <- getsGame (scenarioEncounterDiscard . toAttrs . gameScenario)
      for_ (find ((== EnemyType) . cdCardType . toCardDef) discarded) \c -> do
        removeFromEncounterDiscard c
        putEnemyIntoPlayEngaged you c
      pure t
    _ -> FollowedByNightTheRoadDarkens <$> runMessage msg attrs
