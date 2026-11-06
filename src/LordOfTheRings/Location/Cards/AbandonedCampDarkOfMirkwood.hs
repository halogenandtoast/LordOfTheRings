module LordOfTheRings.Location.Cards.AbandonedCampDarkOfMirkwood (abandonedCampDarkOfMirkwood) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (AllyType, EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype AbandonedCampDarkOfMirkwood = AbandonedCampDarkOfMirkwood LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

abandonedCampDarkOfMirkwood :: LocationCard AbandonedCampDarkOfMirkwood
abandonedCampDarkOfMirkwood = location AbandonedCampDarkOfMirkwood Cards.abandonedCampDarkOfMirkwood

-- | Response: After the players travel to Abandoned Camp, the first player discards the top
-- card of his deck. If that card is an ally, he puts it into play under his control,
-- ready.
instance HasAbilities AbandonedCampDarkOfMirkwood where
  getAbilities (AbandonedCampDarkOfMirkwood a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

-- | Travel: Discard the top card of the encounter deck. If that card is an enemy, add it to
-- the staging area.

instance RunMessage AbandonedCampDarkOfMirkwood where
  runMessage msg l@(AbandonedCampDarkOfMirkwood a) = case msg of
    TravelTo lid | lid == a.id -> do
      discardTopOfEncounterDeck a 1
      AbandonedCampDarkOfMirkwood <$> runMessage msg a
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType EnemyType) cards) addToStagingFromDiscard
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      top <- headMay . (.deck) <$> getPlayer lead
      discardTopOfDeck lead 1
      for_ top \case
        PlayerCard pc | (PlayerCard pc).cardType == AllyType -> do
          removeFromDiscard lead pc.id
          putAllyIntoPlayUnder lead pc
        _ -> pure ()
      pure l
    _ -> AbandonedCampDarkOfMirkwood <$> runMessage msg a
