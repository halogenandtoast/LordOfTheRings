module LordOfTheRings.Location.Cards.SeatOfSeeing (seatOfSeeing) where

import LordOfTheRings.Campaign (CampaignLogUpdate (RemoveFromPool))
import LordOfTheRings.Card (Card (EncounterCard), CardCode, HasCardCode, HasCardDef, toCardCode, toCardDef)
import LordOfTheRings.Entities (Entities)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEntities, gameScenario, heldEntities)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (isBurden)

newtype SeatOfSeeing = SeatOfSeeing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

seatOfSeeing :: LocationCard SeatOfSeeing
seatOfSeeing = location SeatOfSeeing Cards.seatOfSeeing

-- | Immune to player card effects.
instance HasModifiersFor SeatOfSeeing where
  getModifiersFor (SeatOfSeeing a) = modifySelf a [ImmuneToPlayerCardEffects]

{- | Response: When Seat of Seeing is explored, choose a burden card in play, in the
encounter deck, or in the discard pile, and remove it from the game. Shuffle the encounter
deck. (If you are playing campaign mode, remove that card from the Campaign Pool.) One in
another player's staging area comes to this one to leave play.
-}
instance HasAbilities SeatOfSeeing where
  getAbilities (SeatOfSeeing a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

instance RunMessage SeatOfSeeing where
  runMessage msg l@(SeatOfSeeing a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      here <- getsGame (cardsIn False . gameEntities)
      elsewhere <- getsGame (concatMap (cardsIn True) . heldEntities)
      scenario <- getsGame (toAttrs . gameScenario)
      let inDeck = filter (isBurden . toCardDef) scenario.encounterDeck
          inDiscard = filter (isBurden . toCardDef) scenario.encounterDiscard
      lead <- getStageLead
      chooseOneM pid do
        questionLabeled "seatOfSeeing.removeBurden"
        causedBy a
        showing (map EncounterCard $ inDeck <> inDiscard)
        for_ (here <> elsewhere) \(t, code) -> targeting t do
          when ((t, code) `elem` elsewhere) $ placeInStagingAreaOf lead t
          removeFromGame t
          updateCampaignLog $ RemoveFromPool code
        for_ inDeck \c -> targeting (CardIdTarget c.id) do
          removeFromEncounterDeck c
          updateCampaignLog $ RemoveFromPool (toCardCode c)
        for_ inDiscard \c -> targeting (CardIdTarget c.id) do
          removeFromEncounterDiscard c
          updateCampaignLog $ RemoveFromPool (toCardCode c)
      shuffleEncounterDeck
      pure l
    _ -> SeatOfSeeing <$> runMessage msg a

-- | The burdens among the cards, those in the staging area only, if so.
cardsIn :: Bool -> Entities -> [(Target, CardCode)]
cardsIn staged es = pick (toList es.treacheries) <> pick (toList es.objectives) <> pick (toList es.enemies) <> pick (toList es.locations)
 where
  pick :: (HasCardDef c, HasCardCode c, Targetable c, HasField "placement" c Placement) => [c] -> [(Target, CardCode)]
  pick xs = [(toTarget x, toCardCode x) | x <- xs, isBurden (toCardDef x), not staged || x.placement == StagingArea]
