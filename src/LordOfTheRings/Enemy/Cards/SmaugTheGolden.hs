module LordOfTheRings.Enemy.Cards.SmaugTheGolden (smaugTheGolden) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (advanceToStageFour)
import LordOfTheRings.Trait (Trait (Burglar))

newtype SmaugTheGolden = SmaugTheGolden EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

smaugTheGolden :: EnemyCard SmaugTheGolden
smaugTheGolden = enemy SmaugTheGolden Cards.smaugTheGolden

-- | Indestructible. Immune to player card effects. Cannot be engaged.
-- X is the number of non-Burglar characters committed to the quest.
instance HasModifiersFor SmaugTheGolden where
  getModifiersFor (SmaugTheGolden a) = do
    committed <- length <$> selectCharacters CommittedHero CommittedAlly
    burglars <- length <$> selectCharacters (CommittedHero <> HeroWithTrait Burglar) (CommittedAlly <> AllyWithTrait Burglar)
    modifySelf
      a
      $ [ImmuneToPlayerCardEffects, NoEngagementChecks, CannotBeOptionallyEngagedBy AnyPlayer]
      <> [ThreatModifier (committed - burglars) | committed > burglars]

-- | Forced: After the 6th progress token is placed here, the players advance to stage 4A.
instance RunMessage SmaugTheGolden where
  runMessage msg e@(SmaugTheGolden attrs) = case msg of
    -- he cannot be engaged: an effect that would engage him does not move him
    PlaceEnemy eid placement | eid == attrs.id && placement /= StagingArea -> pure e
    PlaceTokens _ (isTarget attrs -> True) Progress n | progress < 6 && progress + n >= 6 -> do
      advanceToStageFour
      SmaugTheGolden <$> runMessage msg attrs
    _ -> SmaugTheGolden <$> runMessage msg attrs
   where
    progress = countTokens Progress attrs.enemyTokens
