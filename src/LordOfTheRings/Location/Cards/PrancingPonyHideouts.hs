module LordOfTheRings.Location.Cards.PrancingPonyHideouts
  (billFernysHouse, staddleHidingHole, combeStorehouse, archetTradingPost, chetwoodCampsite) where

import LordOfTheRings.Card (Card (EncounterCard), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype PrancingPonyHideout = PrancingPonyHideout LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

billFernysHouse, staddleHidingHole, combeStorehouse, archetTradingPost, chetwoodCampsite :: LocationCard PrancingPonyHideout
billFernysHouse = location PrancingPonyHideout Cards.billFernysHouse
staddleHidingHole = location PrancingPonyHideout Cards.staddleHidingHole
combeStorehouse = location PrancingPonyHideout Cards.combeStorehouse
archetTradingPost = location PrancingPonyHideout Cards.archetTradingPost
chetwoodCampsite = location PrancingPonyHideout Cards.chetwoodCampsite

travelEnemy :: LocationAttrs -> CardDef
travelEnemy a
  | toCardCode a == toCardCode Cards.billFernysHouse = Enemies.billFernyMurder
  | toCardCode a == toCardCode Cards.staddleHidingHole = Enemies.greenwayFootpad
  | toCardCode a == toCardCode Cards.combeStorehouse = Enemies.lawlessRuffian
  | toCardCode a == toCardCode Cards.archetTradingPost = Enemies.breeLandInformant
  | otherwise = Enemies.chetwoodOutlaw

instance HasModifiersFor PrancingPonyHideout where
  getModifiersFor (PrancingPonyHideout a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    available <- getsGame $ \g -> let scenario = toAttrs g.scenario
      in scenario.encounterDeck <> scenario.encounterDiscard
        <> [c | EncounterCard c <- scenario.victoryDisplay, toCardCode a == toCardCode Cards.billFernysHouse]
    unless (any (cardMatches $ cardIs $ travelEnemy a) available) $ modifySelf a [CannotBeTraveledTo]

instance RunMessage PrancingPonyHideout where
  runMessage msg l@(PrancingPonyHideout a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      let matcher = cardIs $ travelEnemy a
      if
        | toCardCode a == toCardCode Cards.billFernysHouse -> searchEncounterCardsAndVictoryDisplayToStaging lead a matcher
        | toCardCode a == toCardCode Cards.staddleHidingHole -> searchEncounterDeckAndDiscardToReveal lead a matcher
        | otherwise -> searchEncounterDeckAndDiscard lead a matcher
      pure l
    _ -> PrancingPonyHideout <$> runMessage msg a
