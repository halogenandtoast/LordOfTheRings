module LordOfTheRings.Treachery.Cards.GatheringForces (gatheringForces) where

import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (addRavenCardToStaging, putRavenCardIntoPlayEngaged)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GatheringForces = GatheringForces TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gatheringForces :: TreacheryCard GatheringForces
gatheringForces = treachery GatheringForces Cards.gatheringForces

-- | If any player has 5 or more cards in hand, this effect cannot be canceled.
instance HasModifiersFor GatheringForces where
  getModifiersFor (GatheringForces a) = do
    players <- traverse getPlayer =<< getRemainingPlayers
    when (any ((>= 5) . length . (.hand)) players) $ modifySelf a [WhenRevealedCannotBeCanceled]

-- | When Revealed: Add the top 2 cards of the Raven Deck to the staging area.
-- Shadow: Put the top card of the Raven deck into play, engaged with you.
instance RunMessage GatheringForces where
  runMessage msg t@(GatheringForces attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      replicateM_ 2 addRavenCardToStaging
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      putRavenCardIntoPlayEngaged pid
      pure t
    _ -> GatheringForces <$> runMessage msg attrs
