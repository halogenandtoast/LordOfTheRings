module LordOfTheRings.Location.Cards.DarkColony (darkColony) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonedCharacters)

newtype DarkColony = DarkColony LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkColony :: LocationCard DarkColony
darkColony = location DarkColony Cards.darkColony

instance HasModifiersFor DarkColony where
  getModifiersFor (DarkColony a) = do
    when (a.placement == ActiveLocation)
      $ poisonedCharacters
      >>= traverse_ (\c -> modified a c [CannotReady])
    able <- payingHeroes
    when (null able) $ modifySelf a [CannotBeTraveledTo]

instance RunMessage DarkColony where
  runMessage msg (DarkColony a) = case msg of
    TravelTo lid | lid == a.id -> do
      able <- payingHeroes
      lead <- getLead
      players <- getRemainingPlayers
      eligible <-
        filterM
          (\pid -> selectAny (HeroControlledBy (PlayerWithId pid) <> HeroMatchAny (map HeroWithId able)))
          players
      chooseOrRunOneM lead do
        questionLabeled "choosePlayer"
        causedBy a
        targets eligible \pid -> chooseOrRunOneM pid do
          questionLabeled "darkColony.poison"
          causedBy a
          controlled <- select (HeroControlledBy (PlayerWithId pid) <> HeroMatchAny (map HeroWithId able))
          targets controlled \h -> givePoison h 1
      DarkColony <$> runMessage msg a
    _ -> DarkColony <$> runMessage msg a

-- Travel costs must be paid in full: a hero's controller needs a card to give as poison.
payingHeroes :: HasGame m => m [HeroId]
payingHeroes = do
  heroes <- select (HeroControlledBy AnyPlayer)
  filterM
    ( \hid -> do
        mods <- getModifiers hid
        owner <- (.owner) <$> getHero hid
        cards <- (.deck) <$> getPlayer owner
        pure $ CannotBePoisoned `notElem` mods && notNull cards
    )
    heroes
