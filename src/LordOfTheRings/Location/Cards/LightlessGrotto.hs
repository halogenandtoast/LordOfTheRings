module LordOfTheRings.Location.Cards.LightlessGrotto (lightlessGrotto) where

import LordOfTheRings.Card (CardType (TreacheryType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype LightlessGrotto = LightlessGrotto LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lightlessGrotto :: LocationCard LightlessGrotto
lightlessGrotto = location LightlessGrotto Cards.lightlessGrotto

-- | Forced: After the players commit characters to the quest, discard cards from the
-- encounter deck until a treachery is discarded. Resolve the 'when revealed' effect on that
-- treachery. The players have committed as the staging step begins, as Hunted in Harad
-- reads it.
instance HasAbilities LightlessGrotto where
  getAbilities (LightlessGrotto a) = [mkAbility a 1 $ forced StagingStepBeginsWindow]

instance RunMessage LightlessGrotto where
  runMessage msg l@(LightlessGrotto a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardEncounterCardsUntil a treacheryCard
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (find (cardMatches treacheryCard) cards) resolveWhenRevealedFromDiscard
      pure l
    _ -> LightlessGrotto <$> runMessage msg a
   where
    treacheryCard = CardWithType TreacheryType
