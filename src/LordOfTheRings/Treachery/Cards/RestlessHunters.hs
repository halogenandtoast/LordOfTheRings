module LordOfTheRings.Treachery.Cards.RestlessHunters (restlessHunters) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheHuntForGollum.Helpers (attachClueToMordorEnemy, mordorEnemies)
import LordOfTheRings.Trait (Trait (Clue))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RestlessHunters = RestlessHunters TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

restlessHunters :: TreacheryCard RestlessHunters
restlessHunters = treachery RestlessHunters Cards.restlessHunters

-- | Surge.
-- When Revealed: The first player chooses a Clue card attached to a hero and attaches it to
-- a Mordor enemy in play, if able.
-- Shadow: Damage from this attack must be applied to a hero you control with a Clue card
-- attached, if able.
instance RunMessage RestlessHunters where
  runMessage msg t@(RestlessHunters attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- map toTarget <$> select (HeroWithAttached Clue)
      clues <- filter (\o -> o.placement `elem` map AttachedTo heroes) <$> objectivesWithTrait Clue
      enemies <- mordorEnemies
      lead <- getLead
      unless (null enemies) $ chooseOrRunOneM lead do
        questionLabeled "restlessHunters.clue"
        causedBy attrs
        targets (map toId clues) $ attachClueToMordorEnemy attrs lead
      pure t
    -- the damage goes to that hero for this attack alone: from the defenders or, undefended,
    -- from the enemy
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithAttached Clue)
      chooseOrRunOneM pid do
        questionLabeled "restlessHunters.hero"
        causedBy attrs
        targets heroes \h -> do
          attackModifier attrs eid (UndefendedDamageTo $ toTarget h)
          for_ (filter (/= toTarget h) defenders) \d -> attackModifier attrs d (DamageRedirectedTo $ toTarget h)
      pure t
    _ -> RestlessHunters <$> runMessage msg attrs
