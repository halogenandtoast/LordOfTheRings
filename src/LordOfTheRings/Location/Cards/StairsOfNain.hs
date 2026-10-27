module LordOfTheRings.Location.Cards.StairsOfNain (stairsOfNain) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype StairsOfNain = StairsOfNain LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

stairsOfNain :: LocationCard StairsOfNain
stairsOfNain = location StairsOfNain Cards.stairsOfNain

-- | Travel: The first player must exhaust 1 character he controls to travel here.
instance HasModifiersFor StairsOfNain where
  getModifiersFor (StairsOfNain a) = do
    able <- readyOf =<< getLead
    when (null able) $ modified a a [CannotBeTraveledTo]

-- | Shadow: Defending player must choose and exhaust 1 character he controls.
instance RunMessage StairsOfNain where
  runMessage msg l@(StairsOfNain a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      able <- readyOf lead
      chooseOrRunOneM lead do
        questionLabeled "stairsOfNain.exhaust"
        causedBy a
        targets able exhaust
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseAndExhaust a pid 1
      pure l
    _ -> StairsOfNain <$> runMessage msg a

readyOf :: (HasGame m) => PlayerId -> m [Target]
readyOf p = selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)
