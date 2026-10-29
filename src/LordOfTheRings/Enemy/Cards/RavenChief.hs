module LordOfTheRings.Enemy.Cards.RavenChief (ravenChief) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (removeTimeCounterFromActiveLocation)

newtype RavenChief = RavenChief EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenChief :: EnemyCard RavenChief
ravenChief = enemy RavenChief Cards.ravenChief

-- | Cannot have attachments. While Raven Chief's Camp is in play, Raven Chief cannot be
-- damaged.
instance HasModifiersFor RavenChief where
  getModifiersFor (RavenChief a) = do
    camp <- selectAny (LocationWithCardCode $ toCardCode Locations.ravenChiefsCamp)
    modifySelf a $ CannotHaveAttachments : [CannotTakeDamage | camp]

-- | Forced: When Raven Chief attacks, remove 1 time counter from the active location, if
-- able.
instance HasAbilities RavenChief where
  getAbilities (RavenChief a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage RavenChief where
  runMessage msg e@(RavenChief attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeTimeCounterFromActiveLocation attrs
      pure e
    _ -> RavenChief <$> runMessage msg attrs
