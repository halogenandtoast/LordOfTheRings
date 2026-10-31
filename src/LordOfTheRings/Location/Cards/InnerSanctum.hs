module LordOfTheRings.Location.Cards.InnerSanctum (innerSanctum) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Sorcery))

newtype InnerSanctum = InnerSanctum LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

innerSanctum :: LocationCard InnerSanctum
innerSanctum = location InnerSanctum Cards.innerSanctum

-- | Progress cannot be placed on Inner Sanctum while it is in the staging area.
instance HasModifiersFor InnerSanctum where
  getModifiersFor (InnerSanctum a) = when (a.placement == StagingArea) $ modified a a [ProgressLimit 0]

-- | Travel: Discard the top 3 cards of the encounter deck. Resolve the 'when revealed'
-- effects of each Sorcery card discarded by this effect.
instance RunMessage InnerSanctum where
  runMessage msg l@(InnerSanctum a) = case msg of
    TravelTo lid | lid == a.id -> do
      top <- getsGame (take 3 . (.encounterDeck) . toAttrs . (.scenario))
      discardTopOfEncounterDeck a 3
      for_ top \card -> when (Sorcery `member` (toCardDef card).traits) $ resolveWhenRevealedFromDiscard card
      pure l
    _ -> InnerSanctum <$> runMessage msg a
