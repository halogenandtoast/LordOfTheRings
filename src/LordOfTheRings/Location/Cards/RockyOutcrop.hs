module LordOfTheRings.Location.Cards.RockyOutcrop (rockyOutcrop) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype RockyOutcrop = RockyOutcrop LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

rockyOutcrop :: LocationCard RockyOutcrop
rockyOutcrop = location RockyOutcrop Cards.rockyOutcrop

-- | While Rocky Outcrop is in the staging area, characters cannot be readied by player
-- card effects.
-- Travel: The first player exhausts a hero he controls: with no ready hero, he cannot, and
-- the players cannot travel here.
instance HasModifiersFor RockyOutcrop where
  getModifiersFor (RockyOutcrop a) = do
    when (a.placement == StagingArea) $ modified a GameTarget [CannotBeReadiedByPlayerCards]
    lead <- getLead
    able <- select (HeroControlledBy (PlayerWithId lead) <> ReadyHero)
    when (null able) $ modifySelf a [CannotBeTraveledTo]

instance RunMessage RockyOutcrop where
  runMessage msg l@(RockyOutcrop a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      chooseAndExhaustHero a lead
      pure l
    _ -> RockyOutcrop <$> runMessage msg a
