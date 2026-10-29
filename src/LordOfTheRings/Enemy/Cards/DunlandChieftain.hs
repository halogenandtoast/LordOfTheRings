module LordOfTheRings.Enemy.Cards.DunlandChieftain (dunlandChieftain) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunland))

newtype DunlandChieftain = DunlandChieftain EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlandChieftain :: EnemyCard DunlandChieftain
dunlandChieftain = enemy DunlandChieftain Cards.dunlandChieftain

-- | Forced: After Dunland Chieftain engages a player, discard X cards from the top of the
-- encounter deck where X is the number of cards in the engaged player's hand. Put the
-- topmost Dunland enemy discarded this way into play engaged with that player.
instance HasAbilities DunlandChieftain where
  getAbilities (DunlandChieftain a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage DunlandChieftain where
  runMessage msg e@(DunlandChieftain attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> do
        n <- length . (.hand) <$> getPlayer pid
        when (n > 0) $ discardTopOfEncounterDeckFor attrs (PlayerTarget pid) n
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget pid) cards -> do
      let dunlending c = cdCardType (toCardDef c) == EnemyType && Dunland `member` cdCardTraits (toCardDef c)
      for_ (find dunlending cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged pid card
      pure e
    _ -> DunlandChieftain <$> runMessage msg attrs
