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
-- able. Once the attack is over the shadow reads whether any character it could have
-- destroyed, a defender or, undefended, one of the attacked player's, has left play.
-- ponytail: a character the attack could destroy that left play during it some other way,
-- as a defender discarded by a player card, counts as destroyed (as Morgul Vale's shadow).
instance RunMessage CursedForest where
  runMessage msg l@(CursedForest a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead a (CardMatchAll [CardWithType EnemyType, CardWithTrait SpiritTrait])
      pure l
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      exposed <- if null defenders then charactersControlledBy pid else pure defenders
      afterAttack eid [DoStep 1 (ResolveShadowEffect cid eid pid exposed)]
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ exposed) | cid == a.cardId -> do
      remaining <- selectCharacters AnyHero AnyAlly
      when (any (`notElem` remaining) exposed) do
        enemies <- select AnyEnemy
        timed <- filterM (fmap (> 0) . getTimeCounters . toTarget) enemies
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "cursedForest.removeTimeCounter"
          causedBy a
          targets timed \e -> removeTimeCounters a e 1
      pure l
    _ -> CursedForest <$> runMessage msg a
