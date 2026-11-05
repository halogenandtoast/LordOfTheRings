module LordOfTheRings.Location.Cards.ExpanseOfHollin (expanseOfHollin) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType))

newtype ExpanseOfHollin = ExpanseOfHollin LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

expanseOfHollin :: LocationCard ExpanseOfHollin
expanseOfHollin = location ExpanseOfHollin Cards.expanseOfHollin

instance HasModifiersFor ExpanseOfHollin where
  getModifiersFor (ExpanseOfHollin a) = do
    enemies <- length <$> select AnyEnemy
    modifySelf a [ThreatModifier $ 1 + enemies]

instance HasAbilities ExpanseOfHollin where
  getAbilities (ExpanseOfHollin a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage ExpanseOfHollin where
  runMessage msg l@(ExpanseOfHollin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      discardTopOfEncounterDeck a a.damage
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard (filter (cardMatches $ CardWithType EnemyType) cards)
      pure l
    _ -> ExpanseOfHollin <$> runMessage msg a
