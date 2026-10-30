{- | What the cards of The Weather Hills share: the Orc deck beside the quest deck, and the
Mission objective, Hunting the Orcs or its other side Savage Counter-attack, with the
resource tokens the players keep on it.
-}
module LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers (
  Meta (..),
  getMeta,
  isOrcDeckCard,
  revealFromOrcDeck,
  shuffleOrcDeckIntoEncounterDeck,
  getMissionObjective,
  missionResources,
  orcAmbush,
) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardType (EnemyType), EncounterCard, cdCardTraits, cdCardType, cdEncounterSet, toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.EncounterSet (EncounterSet (AngmarOrcs))
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardWithCode))
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.Types (objectiveTokens)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs)
import LordOfTheRings.Token
import LordOfTheRings.Trait (Trait (Mission))

-- | The scenario's own state: the Orc deck, top first.
newtype Meta = Meta {orcDeck :: [EncounterCard]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta []) $ parseMaybe parseJSON attrs.meta

-- | The Orc deck (rules insert): each enemy of Angmar Orcs, and each copy of Concealed
-- Orc-camp.
isOrcDeckCard :: EncounterCard -> Bool
isOrcDeckCard card =
  (cdCardType def == EnemyType && cdEncounterSet def == Just AngmarOrcs)
    || toCardCode card == toCardCode Locations.concealedOrcCamp
 where
  def = toCardDef card

-- | Reveal the top cards of the Orc deck, each staged as if just revealed from the
-- encounter deck.
revealFromOrcDeck :: ReverseQueue m => Int -> m ()
revealFromOrcDeck n = when (n > 0) $ push $ ScenarioSpecific "orcDeck.reveal" (toJSON n)

shuffleOrcDeckIntoEncounterDeck :: ReverseQueue m => m ()
shuffleOrcDeckIntoEncounterDeck = push $ ScenarioSpecific "orcDeck.shuffleIn" Null

-- | The Mission objective in play: Hunting the Orcs, or Savage Counter-attack once it flips.
getMissionObjective :: HasGame m => m (Maybe ObjectiveId)
getMissionObjective =
  getsGame
    (fmap toId . find ((Mission `elem`) . cdCardTraits . toCardDef) . toList . entitiesObjectives . gameEntities)

-- | The resource tokens on the Mission objective.
missionResources :: HasGame m => m Int
missionResources = do
  objectives <- getsGame (entitiesObjectives . gameEntities)
  mission <- getMissionObjective
  pure $ maybe 0 (countTokens Resource . objectiveTokens . toAttrs) (mission >>= (`Map.lookup` objectives))

-- | Orc Ambush, the Angmar Orcs side quest set aside at setup: its card is another set's,
-- so it is named by its code.
orcAmbush :: CardMatcher
orcAmbush = CardWithCode "09046"
