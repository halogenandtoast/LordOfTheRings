module LordOfTheRings.Objective.Cards.SearchingForAWayOut (searchingForAWayOut) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (isObjectiveLocation, objectiveLocationsInPlay)

newtype SearchingForAWayOut = SearchingForAWayOut ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

searchingForAWayOut :: ObjectiveCard SearchingForAWayOut
searchingForAWayOut = objective SearchingForAWayOut Cards.searchingForAWayOut

{- | Each objective-location, and each card guarding one, is immune to player card effects.
Forced: When progress would be placed on the main quest, if there are no objective-
locations in play, discard an equal number of cards from the encounter deck instead: the
progress comes here instead, and is spent so.
-}
instance HasModifiersFor SearchingForAWayOut where
  getModifiersFor (SearchingForAWayOut a) = do
    objectiveLocations <- getsGame (filter isObjectiveLocation . toList . (.entities.locations))
    let guards = [cid | l <- objectiveLocations, AttachedTo (CardIdTarget cid) <- [l.placement]]
    for_ objectiveLocations \l -> modified a l [ImmuneToPlayerCardEffects]
    enemies <- getsGame (filter ((`elem` guards) . (.cardId) . toAttrs) . toList . (.entities.enemies))
    for_ enemies \e -> modified a e [ImmuneToPlayerCardEffects]
    locations <- getsGame (filter ((`elem` guards) . (.cardId) . toAttrs) . toList . (.entities.locations))
    for_ locations \l -> modified a l [ImmuneToPlayerCardEffects]
    noneInPlay <- null <$> objectiveLocationsInPlay
    when noneInPlay do
      mains <- getMainQuests
      for_ mains \q -> modified a q [ProgressPlacedOnInstead (toTarget a)]

-- | Then, put the topmost objective-location in the encounter discard pile into play.
instance RunMessage SearchingForAWayOut where
  runMessage msg o@(SearchingForAWayOut attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      discardTopOfEncounterDeck attrs n
      pure o
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ _ -> do
      discarded <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      for_ (find isObjectiveLocation discarded) addToStagingFromDiscard
      pure o
    _ -> SearchingForAWayOut <$> runMessage msg attrs
