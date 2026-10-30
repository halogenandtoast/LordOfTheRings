module LordOfTheRings.Location.Cards.HelmOfTheStormcaller (helmOfTheStormcaller) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype HelmOfTheStormcaller = HelmOfTheStormcaller LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

helmOfTheStormcaller :: LocationCard HelmOfTheStormcaller
helmOfTheStormcaller = location HelmOfTheStormcaller Cards.helmOfTheStormcaller

-- | Immune to player card effects.
-- Helm of the Stormcaller gets +1 quest point for each resource token on a Raider enemy in
-- play.
instance HasModifiersFor HelmOfTheStormcaller where
  getModifiersFor (HelmOfTheStormcaller a) = do
    raiders <- traverse getEnemy =<< select (EnemyWithTrait Raider)
    let n = sum [countTokens Resource (toAttrs r).enemyTokens | r <- raiders]
    modifySelf a $ ImmuneToPlayerCardEffects : [QuestPointModifier n | n > 0]

-- | Forced: When Helm of the Stormcaller becomes the active location, add Captain Sahír and
-- Na'asiyah to the staging area from out of play (enemy side faceup).
instance HasAbilities HelmOfTheStormcaller where
  getAbilities (HelmOfTheStormcaller a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow When (LocationWithId a.id)]

instance RunMessage HelmOfTheStormcaller where
  runMessage msg l@(HelmOfTheStormcaller attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      addSetAsideToStaging (cardIs Enemies.captainSahir)
      addSetAsideToStaging (cardIs Enemies.naasiyah)
      pure l
    _ -> HelmOfTheStormcaller <$> runMessage msg attrs
