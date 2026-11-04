module LordOfTheRings.Enemy.Cards.ThaneUlchorTheFortressOfNurn (thaneUlchorTheFortressOfNurn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (castleX)

newtype ThaneUlchorTheFortressOfNurn = ThaneUlchorTheFortressOfNurn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

thaneUlchorTheFortressOfNurn :: EnemyCard ThaneUlchorTheFortressOfNurn
thaneUlchorTheFortressOfNurn = enemy ThaneUlchorTheFortressOfNurn Cards.thaneUlchorTheFortressOfNurn

-- | Immune to player card effects. X is 1 more than the number of Castle side quests in the
-- victory display.
instance HasModifiersFor ThaneUlchorTheFortressOfNurn where
  getModifiersFor (ThaneUlchorTheFortressOfNurn a) = do
    x <- castleX
    modifySelf a [ImmuneToPlayerCardEffects, ThreatModifier x]

-- | Forced: After Thane Ulchor attacks, reveal the top card of the encounter deck.
instance HasAbilities ThaneUlchorTheFortressOfNurn where
  getAbilities (ThaneUlchorTheFortressOfNurn a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage ThaneUlchorTheFortressOfNurn where
  runMessage msg e@(ThaneUlchorTheFortressOfNurn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure e
    _ -> ThaneUlchorTheFortressOfNurn <$> runMessage msg attrs
