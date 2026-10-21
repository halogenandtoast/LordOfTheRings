module LordOfTheRings.Location.Cards.TheEastBank (theEastBank) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))
import LordOfTheRings.Trait (Trait (Clue))

newtype TheEastBank = TheEastBank LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEastBank :: LocationCard TheEastBank
theEastBank = location TheEastBank Cards.theEastBank

-- | While The East Bank is the active location, ally cards cost 1 additional matching
-- resource to play from hand.
instance HasModifiersFor TheEastBank where
  getModifiersFor (TheEastBank a) = when (a.placement == ActiveLocation) $ modified a GameTarget [IncreaseCost (CardWithType AllyType) 1]

-- | Shadow: If you do not control at least 1 hero with a Clue card attached, return this
-- enemy to the staging area after its attack resolves.
instance RunMessage TheEastBank where
  runMessage msg l@(TheEastBank a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      clues <- selectAny (HeroControlledBy (PlayerWithId pid) <> HeroWithAttached Clue)
      unless clues $ afterAttack eid [PlaceEnemy eid StagingArea]
      pure l
    _ -> TheEastBank <$> runMessage msg a
