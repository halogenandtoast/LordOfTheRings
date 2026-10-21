module LordOfTheRings.Ally.Cards.WestRoadTraveller (westRoadTraveller) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (..))

newtype WestRoadTraveller = WestRoadTraveller AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

westRoadTraveller :: AllyCard WestRoadTraveller
westRoadTraveller = ally WestRoadTraveller Cards.westRoadTraveller

-- | Response: After you play West Road Traveller from your hand, switch the active
-- location with any other location in the staging area.
instance HasAbilities WestRoadTraveller where
  getAbilities (WestRoadTraveller a) =
    [ restricted a 1 (exists IsActiveLocation <> exists LocationInStagingArea)
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    ]

instance RunMessage WestRoadTraveller where
  runMessage msg s@(WestRoadTraveller a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      active <- select IsActiveLocation
      staged <- select LocationInStagingArea
      chooseOrRunOneM pid do
        questionLabeled "westRoadTraveller.switch"
        causedBy a
        targets staged \l -> do
          for_ active \x -> push $ PlaceLocation x StagingArea
          push $ PlaceLocation l ActiveLocation
      pure s
    _ -> WestRoadTraveller <$> runMessage msg a
