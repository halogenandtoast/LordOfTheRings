module LordOfTheRings.Location.Cards.TheSerpentsRedoubt (theSerpentsRedoubt) where

import LordOfTheRings.Ally.Types (AllyAttrs (allyCardId))
import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea), placementController)

newtype TheSerpentsRedoubt = TheSerpentsRedoubt LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSerpentsRedoubt :: LocationCard TheSerpentsRedoubt
theSerpentsRedoubt = location TheSerpentsRedoubt Cards.theSerpentsRedoubt

{- | While The Serpent's Redoubt is in the staging area, it gains: "Forced: After a player
plays an ally from his hand, either remove 1 progress from the main quest, or exhaust that
ally." The removal is offered only when it can be made in full (FAQ 1.44).
Travel: The first player searches the encounter deck and discard pile for an enemy and puts
it into play engaged with him. Shuffle the encounter deck.
-}
instance RunMessage TheSerpentsRedoubt where
  runMessage msg (TheSerpentsRedoubt a) = case msg of
    CheckWindows ws | a.placement == StagingArea -> do
      let played = [cid | Window After (PlayedFromHand (CardIdTarget cid)) <- ws]
      allies <- getsGame (filter ((`elem` played) . allyCardId) . map toAttrs . toList . (.entities.allies))
      for_ allies \ally -> for_ (placementController ally.placement) \pid ->
        chooseOrRunOneM pid do
          questionLabeled "theSerpentsRedoubt.choose"
          causedBy a
          removeMainQuestProgressLabeled a 1
          labeled "theSerpentsRedoubt.exhaust" $ exhaust ally.id
      TheSerpentsRedoubt <$> runMessage msg a
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscardToEngage lead a (CardWithType EnemyType)
      TheSerpentsRedoubt <$> runMessage msg a
    _ -> TheSerpentsRedoubt <$> runMessage msg a
