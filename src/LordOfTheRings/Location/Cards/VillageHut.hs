module LordOfTheRings.Location.Cards.VillageHut (villageHut) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (AllyType, EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Ally.Types (allyCardId)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype VillageHut = VillageHut LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

villageHut :: LocationCard VillageHut
villageHut = location VillageHut Cards.villageHut

-- | Response: After the players travel to Village Hut, the first player discards the top
-- card of his deck. If that card is an ally, he puts it into play under his control,
-- exhausted.
instance HasAbilities VillageHut where
  getAbilities (VillageHut a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

-- | Travel: Discard the top card of the encounter deck. If that card is an enemy, add it to
-- the staging area.
-- The ally put into play is exhausted once it is in play, found by its card.
instance RunMessage VillageHut where
  runMessage msg l@(VillageHut a) = case msg of
    TravelTo lid | lid == a.id -> do
      discardTopOfEncounterDeck a 1
      VillageHut <$> runMessage msg a
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
          doStep 1 (Exhaust $ CardIdTarget pc.id)
        _ -> pure ()
      pure l
    DoStep 1 (Exhaust (CardIdTarget cid)) -> do
      allies <- getsGame (filter ((== cid) . allyCardId . toAttrs) . toList . (.entities.allies))
      for_ allies exhaust
      pure l
    _ -> VillageHut <$> runMessage msg a
