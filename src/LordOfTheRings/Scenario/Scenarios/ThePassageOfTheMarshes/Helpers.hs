-- | What The Passage of the Marshes' cards share: Sméagol, the Undead enemies, the mire
-- tokens, and the campaign card's resolution.
module LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (
  smeagol,
  readySmeagol,
  firstPlayerControlsSmeagol,
  smeagolTravelModifiers,
  exhaustSmeagolToTravel,
  undeadEnemy,
  engagedPlayerCannotReduceThreat,
  placeMireOnActiveLocation,
  searchDeckAndDiscardFor,
  locationsInVictoryDisplay,
  passageOfTheMarshesResolution,
) where

import Data.List (elemIndex)
import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType, LocationType), EncounterCard, cdCardType, toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.Enemy.Types (EnemyAttrs)
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool, AdjustSeatThreatPenalty), PoolCard (..))
import LordOfTheRings.Id
import LordOfTheRings.Location.Types (LocationAttrs)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (CannotBeTraveledTo, CannotReduceThreat))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Prelude
import LordOfTheRings.Queue
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Undead))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

-- | Sméagol, objective-ally side faceup.
smeagol :: AllyMatcher
smeagol = AllyWithCardCode (toCardCode Objectives.smeagolTheLandOfShadow)

readySmeagol :: AllyMatcher
readySmeagol = smeagol <> ReadyAlly

-- | "The players cannot advance unless the first player controls Sméagol."
firstPlayerControlsSmeagol :: HasGame m => m Bool
firstPlayerControlsSmeagol = do
  lead <- getLead
  selectAny (smeagol <> AllyControlledBy (PlayerWithId lead))

-- | "Travel: Exhaust Sméagol to travel here": not without a ready Sméagol to exhaust.
smeagolTravelModifiers :: HasModifiersM m => LocationAttrs -> m ()
smeagolTravelModifiers a = do
  none <- selectNone readySmeagol
  when none $ modified a a [CannotBeTraveledTo]

exhaustSmeagolToTravel :: ReverseQueue m => m ()
exhaustSmeagolToTravel = traverse_ exhaust =<< select readySmeagol

undeadEnemy :: CardMatcher
undeadEnemy = CardMatchAll [CardWithType EnemyType, CardWithTrait Undead]

-- | "The engaged player cannot reduce his threat."
engagedPlayerCannotReduceThreat :: HasModifiersM m => EnemyAttrs -> m ()
engagedPlayerCannotReduceThreat a = case a.placement of
  EngagedWith pid -> modified a (PlayerTarget pid) [CannotReduceThreat]
  _ -> pure ()

-- | "Place 1 mire token on the active location": a resource, as the Mire keyword counts
-- them (The Land of Shadow rules insert).
placeMireOnActiveLocation :: (ReverseQueue m, Sourceable source) => source -> m ()
placeMireOnActiveLocation source = traverse_ (\l -> placeResources source l 1) =<< select IsActiveLocation

{- | The player searches the encounter deck and discard pile for a matching card, chooses one
of those found, and hands it on; then the encounter deck is shuffled, as "search the
encounter deck and discard pile for an Undead enemy" reads.
-}
searchDeckAndDiscardFor
  :: ReverseQueue m => PlayerId -> Text -> CardMatcher -> (EncounterCard -> QueueT Message m ()) -> m ()
searchDeckAndDiscardFor pid question matcher f = do
  deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
  discardPile <- getEncounterDiscard
  let found = [(c, removeFromEncounterDeck c) | c <- deck, cardMatches matcher c] <> [(c, removeFromEncounterDiscard c) | c <- discardPile, cardMatches matcher c]
  chooseOrRunOneM pid do
    questionLabeled question
    showing [EncounterCard c | (c, _) <- found]
    for_ found \(c, remove) -> targeting (CardIdTarget c.id) (remove >> f c)
  shuffleEncounterDeck

locationsInVictoryDisplay :: HasGame m => m [EncounterCard]
locationsInVictoryDisplay =
  getsGame \g -> [c | EncounterCard c <- (toAttrs g.scenario).victoryDisplay, cdCardType (toCardDef c) == LocationType]

{- | The campaign card's resolution: "Each player must choose: either take a permanent +2
starting threat penalty, or earn 1 copy of The Searching Eye. Record each player's
choice in the campaign log." Asked as the last stage is defeated, before the game is won:
once it is over no question can be asked, and what it writes to the log is kept as the
scenario is won.
-}
passageOfTheMarshesResolution :: ReverseQueue m => m ()
passageOfTheMarshesResolution = do
  players <- getRemainingPlayers
  for_ players \pid -> do
    seat <- elemIndex pid <$> getsGame (.playerOrder)
    for_ seat \s ->
      chooseOneM pid do
        questionLabeled "thePassageOfTheMarshes.resolution"
        labeled "thePassageOfTheMarshes.threatPenalty" $ updateCampaignLog (AdjustSeatThreatPenalty s 2)
        labeled "thePassageOfTheMarshes.searchingEye"
          $ updateCampaignLog
          $ AddToPool
          $ PoolCard (toCardCode Treacheries.theSearchingEye) (Just s) Nothing
