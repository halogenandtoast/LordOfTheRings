module LordOfTheRings.Objective.Cards.Bribes (bribes) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype Bribes = Bribes ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

bribes :: ObjectiveCard Bribes
bribes = objective Bribes Cards.bribes

-- | Add +1 to the total Threat in the staging area for each resource here.
instance HasModifiersFor Bribes where
  getModifiersFor (Bribes a) = when (a.resources > 0) $ modified a GameTarget [StagingThreatBonus a.resources]

-- | Forced: When the active location is explored, discard the top X cards of each player's
-- deck, when X is that location's Threat. Then, place 1 resource here for each event
-- discarded this way.
instance HasAbilities Bribes where
  getAbilities (Bribes a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow When]

-- | The location is still in play as it is explored: its Threat is read then. The events
-- are counted from the cards about to be discarded.
instance RunMessage Bribes where
  runMessage msg o@(Bribes attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [lid | Window _ (ActiveLocationExplored lid) <- ws] \lid -> do
        x <- getLocationThreat lid
        players <- getRemainingPlayers
        events <- for players \pid -> do
          deck <- (.deck) <$> getPlayer pid
          discardTopOfDeck pid x
          pure $ length $ filter ((== EventType) . (.cardType)) (take x deck)
        when (sum events > 0) $ placeResources attrs attrs (sum events)
      pure o
    _ -> Bribes <$> runMessage msg attrs
