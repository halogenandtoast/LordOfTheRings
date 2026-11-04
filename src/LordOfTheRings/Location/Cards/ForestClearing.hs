module LordOfTheRings.Location.Cards.ForestClearing (forestClearing) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ForestClearing = ForestClearing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestClearing :: LocationCard ForestClearing
forestClearing = location ForestClearing Cards.forestClearing

-- | Forced: When Forest Clearing is explored, discard the top card of the encounter deck. If
-- that card is an enemy, add that enemy to the staging area.
instance HasAbilities ForestClearing where
  getAbilities (ForestClearing a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | Shadow: Discard an attachment you control.
instance RunMessage ForestClearing where
  runMessage msg l@(ForestClearing a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardTopOfEncounterDeck a 1
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType EnemyType) cards) addToStagingFromDiscard
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard (CardIdSource cid) pid attachments
      pure l
    _ -> ForestClearing <$> runMessage msg a
