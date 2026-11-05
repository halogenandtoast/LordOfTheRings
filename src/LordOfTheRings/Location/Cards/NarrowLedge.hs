module LordOfTheRings.Location.Cards.NarrowLedge (narrowLedge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype NarrowLedge = NarrowLedge LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

narrowLedge :: LocationCard NarrowLedge
narrowLedge = location NarrowLedge Cards.narrowLedge

{- | The limit applies separately to each player, and falls away if the location stops
being active or a committed character is removed from the quest.
-}
instance HasModifiersFor NarrowLedge where
  getModifiersFor (NarrowLedge a) = when (a.placement == ActiveLocation) do
    players <- getRemainingPlayers
    for_ players \p -> do
      committed <-
        selectCharacters
          (HeroControlledBy (PlayerWithId p) <> CommittedHero)
          (AllyControlledBy (PlayerWithId p) <> CommittedAlly)
      when (length committed >= 3) do
        characters <- charactersControlledBy p
        for_ characters \c -> modified a c [CannotCommitToQuest]

instance RunMessage NarrowLedge where
  runMessage msg l@(NarrowLedge attrs) = case msg of
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      shuffleEncounterDiscardIntoDeck (cardIs Treacheries.gallopingBoulders)
      pure l
    _ -> NarrowLedge <$> runMessage msg attrs
