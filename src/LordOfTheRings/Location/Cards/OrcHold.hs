module LordOfTheRings.Location.Cards.OrcHold (orcHold) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Orc))

newtype OrcHold = OrcHold LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

orcHold :: LocationCard OrcHold
orcHold = location OrcHold Cards.orcHold

orcEnemy :: CardMatcher
orcEnemy = CardWithType EnemyType <> CardWithTrait Orc

-- | While Orc-hold is in the staging area, progress cannot be placed on other locations in
-- the staging area.
instance HasModifiersFor OrcHold where
  getModifiersFor (OrcHold a) = when (a.placement == StagingArea) do
    others <- select (LocationInStagingArea <> not_ (LocationWithId a.id))
    for_ others \l -> modified a l [ProgressLimit 0]

-- | Forced: After Orc-hold becomes the active location, shuffle the encounter discard pile
-- into the encounter deck. Discard cards from the top until an Orc enemy is discarded. Put
-- that enemy into play engaged with the first player.
instance HasAbilities OrcHold where
  getAbilities (OrcHold a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | The cards are discarded once the shuffled deck is there to discard from.
instance RunMessage OrcHold where
  runMessage msg l@(OrcHold a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      discardEncounterCardsUntil a orcEnemy
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      lead <- getLead
      for_ (find (cardMatches orcEnemy) cards) (putEnemyIntoPlayEngagedFromDiscard lead)
      pure l
    _ -> OrcHold <$> runMessage msg a
