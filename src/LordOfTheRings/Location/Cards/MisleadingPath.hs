module LordOfTheRings.Location.Cards.MisleadingPath (misleadingPath) where

import LordOfTheRings.Card (CardType (AllyType), toCardDef, cdCardType)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype MisleadingPath = MisleadingPath LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

misleadingPath :: LocationCard MisleadingPath
misleadingPath = location MisleadingPath Cards.misleadingPath

instance HasModifiersFor MisleadingPath where
  getModifiersFor (MisleadingPath a) = when (a.placement == StagingArea) do
    unlessM (selectAny IsActiveLocation) $ modifySelf a [MustBeTraveledTo]

instance RunMessage MisleadingPath where
  runMessage msg l@(MisleadingPath a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      player <- getPlayer pid
      for_ player.hand \card -> when (cdCardType (toCardDef card) == AllyType) $ discardFromHand pid card
      pure l
    _ -> MisleadingPath <$> runMessage msg a
