module LordOfTheRings.Location.Cards.CursedForest (cursedForest) where

import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (SpiritTrait))

newtype CursedForest = CursedForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedForest :: LocationCard CursedForest
cursedForest = location CursedForest Cards.cursedForest

-- | Travel: Search the encounter deck and discard pile for a Spirit enemy and add it to the
-- staging area.
-- Shadow: If this attack destroys a character, remove a time counter from an enemy, if
-- able.
instance RunMessage CursedForest where
  runMessage msg l@(CursedForest a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead a (CardMatchAll [CardWithType EnemyType, CardWithTrait SpiritTrait])
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      enemies <- select AnyEnemy
      timed <- filterM (fmap (> 0) . getTimeCounters . toTarget) enemies
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "cursedForest.removeTimeCounter"
        causedBy a
        targets timed \e -> removeTimeCounters a e 1
      pure l
    _ -> CursedForest <$> runMessage msg a
