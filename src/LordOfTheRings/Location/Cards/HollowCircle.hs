module LordOfTheRings.Location.Cards.HollowCircle (hollowCircle) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Wight))

newtype HollowCircle = HollowCircle LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hollowCircle :: LocationCard HollowCircle
hollowCircle = location HollowCircle Cards.hollowCircle

wightCards :: CardMatcher
wightCards = CardWithType EnemyType <> CardWithTrait Wight

instance HasModifiersFor HollowCircle where
  getModifiersFor (HollowCircle a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    players <- getStagePlayers
    remaining <- getRemainingPlayers
    available <- getsGame $ \g ->
      let scenario = toAttrs g.scenario
       in length $ filter (cardMatches wightCards) (scenario.encounterDeck <> scenario.encounterDiscard)
    -- Core rules, Travel Effects: the entire travel cost must be payable.
    when (available < length (filter (`elem` remaining) players)) $ modifySelf a [CannotBeTraveledTo]

instance RunMessage HollowCircle where
  runMessage msg l@(HollowCircle a) = case msg of
    TravelTo lid | lid == a.id -> do
      players <- getStagePlayers
      eachPlayer $ \pid -> when (pid `elem` players) $
        searchEncounterDeckAndDiscardToEngage pid a wightCards
      pure l
    _ -> HollowCircle <$> runMessage msg a
