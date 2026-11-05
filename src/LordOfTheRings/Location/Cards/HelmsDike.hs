module LordOfTheRings.Location.Cards.HelmsDike (helmsDike) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype HelmsDike = HelmsDike LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

helmsDike :: LocationCard HelmsDike
helmsDike = location HelmsDike Cards.helmsDike

instance HasModifiersFor HelmsDike where
  getModifiersFor (HelmsDike a) = when (a.placement == ActiveLocation) do
    enemies <- select AnyEnemy
    for_ enemies \eid -> modified a eid [EngagementCostModifier (-10)]

instance HasAbilities HelmsDike where
  getAbilities (HelmsDike a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage HelmsDike where
  runMessage msg l@(HelmsDike a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      discardEncounterCardsUntil a (CardWithType EnemyType)
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (find (cardMatches $ CardWithType EnemyType) cards) addToStagingFromDiscard
      pure l
    _ -> HelmsDike <$> runMessage msg a
