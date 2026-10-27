module LordOfTheRings.Location.Cards.StairFalls (stairFalls) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype StairFalls = StairFalls LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

stairFalls :: LocationCard StairFalls
stairFalls = location StairFalls Cards.stairFalls

-- | Travel: The first player must exhaust 2 characters to travel here.
instance HasModifiersFor StairFalls where
  getModifiersFor (StairFalls a) = do
    lead <- getLead
    able <- selectCharacters (HeroControlledBy (PlayerWithId lead) <> ReadyHero) (AllyControlledBy (PlayerWithId lead) <> ReadyAlly)
    when (length able < 2) $ modified a a [CannotBeTraveledTo]

-- | Shadow: Remove 1 progress token from the current quest.
instance RunMessage StairFalls where
  runMessage msg l@(StairFalls a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      chooseAndExhaust a lead 2
      pure l
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      removeProgressFromQuest a 1
      pure l
    _ -> StairFalls <$> runMessage msg a
