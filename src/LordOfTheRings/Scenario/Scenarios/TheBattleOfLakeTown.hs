module LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown (theBattleOfLakeTown) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ability (WindowMatcher (PhaseBeginsWindow), forced, mkAbility)
import LordOfTheRings.Matcher (getLead, getDamage)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (smaugEnemies)
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Meta
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Timing (Timing (After))

newtype TheBattleOfLakeTown = TheBattleOfLakeTown ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBattleOfLakeTown :: Scenario
theBattleOfLakeTown = Scenario $ scenario TheBattleOfLakeTown "the-battle-of-lake-town" "The Battle of Lake-town"

instance HasAbilities TheBattleOfLakeTown where
  getAbilities (TheBattleOfLakeTown a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

instance RunMessage TheBattleOfLakeTown where
  runMessage msg (TheBattleOfLakeTown original) = do
    -- The rule sheet reshuffles the separate discard whenever the deck is empty.
    let oldMeta = metaOf original
    m <- if null (smaugDeck oldMeta) && notNull (smaugDiscard oldMeta)
      then do
        cards <- shuffleM (smaugDiscard oldMeta)
        pure $ Meta cards []
      else pure oldMeta
    let a = withMeta m original
    case msg of
      UseCardAbility _ (isSource a -> True) 1 _ -> do
        lead <- getLead
        chooseOneM lead do
          questionLabeled "lakeTown.smaugDestination"
          causedBy a
          labeled "lakeTown.smaugStaging" $ push $ ScenarioSpecific "lakeTown.drawSmaug" Null
          labeled "lakeTown.smaugEngaged" $ push $ ScenarioSpecific "lakeTown.drawSmaug" (toJSON $ Just lead)
        pure $ TheBattleOfLakeTown a
      ScenarioSpecific "lakeTown.drawSmaug" value
        | Just engaging <- parseMaybe parseJSON value -> do
            case smaugDeck m of
              [] -> pure $ TheBattleOfLakeTown a
              card : rest -> do
                previous <- smaugEnemies
                damage <- sum <$> traverse (getDamage . EnemyTarget) previous
                for_ previous $ discard a
                -- Initial damage is in place before the new card's revelation, so
                -- Magnificent can move three of those tokens to Lake-town.
                push $ RevealCardWithDamage engaging card damage
                pure $ TheBattleOfLakeTown $ withMeta (m {smaugDeck = rest}) a
      _ -> TheBattleOfLakeTown <$> runMessage msg a
