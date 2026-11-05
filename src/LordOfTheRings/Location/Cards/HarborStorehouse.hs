module LordOfTheRings.Location.Cards.HarborStorehouse (harborStorehouse) where

import LordOfTheRings.Card (CardType (LocationType), cdCardType, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Thug))

newtype HarborStorehouse = HarborStorehouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

harborStorehouse :: LocationCard HarborStorehouse
harborStorehouse = location HarborStorehouse Cards.harborStorehouse

-- | Forced: Each time a location is discarded from the top of the encounter deck, raise
-- each player's threat by 1.
-- Read off the discard itself, as no window opens for a card discarded from the
-- encounter deck.
-- Shadow: Deal attacking enemy an additional shadow card for each Thug enemy in play.
instance RunMessage HarborStorehouse where
  runMessage msg l@(HarborStorehouse a) = case msg of
    DiscardedTopOfEncounterDeck _ _ cards
      | locations@(_ : _) <- [c | c <- cards, cdCardType (toCardDef c) == LocationType] -> do
          eachPlayer \p -> encounterRaisesThreat p (length locations)
          HarborStorehouse <$> runMessage msg a
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      thugs <- length <$> select (EnemyWithTrait Thug)
      when (thugs > 0) do
        replicateM_ thugs $ dealShadowCard eid
        -- the new resolution turns over every shadow card still face down
        resolveNewShadowCards eid pid defenders
      pure l
    _ -> HarborStorehouse <$> runMessage msg a
