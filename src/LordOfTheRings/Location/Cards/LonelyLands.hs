module LordOfTheRings.Location.Cards.LonelyLands (lonelyLands) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (EventType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype LonelyLands = LonelyLands LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

lonelyLands :: LocationCard LonelyLands
lonelyLands = location LonelyLands Cards.lonelyLands

-- | Lonely Lands gets +2 threat for each facedown card under it.
instance HasModifiersFor LonelyLands where
  getModifiersFor (LonelyLands a) = do
    let n = length a.underneath
    when (n > 0) $ modifySelf a [ThreatModifier (2 * n)]

-- | While Lonely Lands is in the staging area, it gains: "Forced: After an event is played
-- during the quest phase, place it facedown under Lonely Lands." The event has resolved by
-- then, so it is taken from its owner's discard pile.
instance HasAbilities LonelyLands where
  getAbilities (LonelyLands a) =
    [ restricted a 1 (DuringPhase QuestPhase) $ forced $ CardPlayedWindow After (CardWithType EventType)
    | a.placement == StagingArea
    ]

-- | The cards under it go to their owners' discard piles as it leaves play.
instance RunMessage LonelyLands where
  runMessage msg l@(LonelyLands a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      placeUnderneath a [PlayerCard card | Window After (PlayedCard _ card) <- ws]
      pure l
    Discard (isTarget a -> True) -> release
    RemoveFromGame (isTarget a -> True) -> release
    ShuffleIntoEncounterDeck (isTarget a -> True) -> release
    ReturnToTopOfEncounterDeck (isTarget a -> True) -> release
    ReturnToBottomOfEncounterDeck (isTarget a -> True) -> release
    _ -> LonelyLands <$> runMessage msg a
   where
    release = do
      for_ [pc | PlayerCard pc <- a.underneath] (push . PutInDiscard)
      LonelyLands <$> runMessage msg (a {locationUnderneath = []})
