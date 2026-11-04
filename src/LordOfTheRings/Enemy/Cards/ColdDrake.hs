module LordOfTheRings.Enemy.Cards.ColdDrake (coldDrake) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWitheredHeath.Helpers (dragonSignsInVictoryDisplay)

newtype ColdDrake = ColdDrake EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

coldDrake :: EnemyCard ColdDrake
coldDrake = enemy ColdDrake Cards.coldDrake

-- | Cannot have attachments or take non-combat damage. Cold-Drake gets +1 threat and +1
-- defense for each Dragon Sign in the victory display.
instance HasModifiersFor ColdDrake where
  getModifiersFor (ColdDrake a) = do
    x <- dragonSignsInVictoryDisplay
    modifySelf a $ [CannotHaveAttachments, CannotTakeNonCombatDamage] <> (if x > 0 then [ThreatModifier x, DefenseModifier x] else [])

{- | Forced: After Cold-Drake is dealt a shadow card with no shadow effect, the defending
character cannot ready until the end of the round. Read as its shadow cards resolve, as
Werewolf's, when there is a defending character to name.
Set aside by stage 3A, it comes back at stage 4A with the damage it kept.
-}
instance RunMessage ColdDrake where
  runMessage msg (ColdDrake attrs) = case msg of
    ResolveShadow eid _ defenders | eid == attrs.id -> do
      let faceDown = filter ((`notElem` attrs.enemyShadowRevealed) . (.id)) attrs.shadowCards
      when (any (not . cdShadowEffect . toCardDef) faceDown)
        $ for_ defenders \d -> roundModifier attrs d CannotReady
      ColdDrake <$> runMessage msg attrs
    ScenarioSpecific "witheredHeath.drakeReturns" (parseMaybe parseJSON -> Just n) ->
      pure $ ColdDrake $ attrs & tokensL %~ addTokens Damage n
    _ -> ColdDrake <$> runMessage msg attrs
