module LordOfTheRings.Enemy.Cards.AnduinTrollSpawn (anduinTrollSpawn) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype AnduinTrollSpawn = AnduinTrollSpawn EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

anduinTrollSpawn :: EnemyCard AnduinTrollSpawn
anduinTrollSpawn = enemy AnduinTrollSpawn Cards.anduinTrollSpawn

hillTrolls :: EnemyMatcher
hillTrolls = EnemyWithCardCode (toCardCode Cards.hillTroll)

-- | If there is no Hill Troll in play, Anduin Troll Spawn gains surge.
instance HasModifiersFor AnduinTrollSpawn where
  getModifiersFor (AnduinTrollSpawn a) = do
    none <- selectNone hillTrolls
    when none $ modifySelf a [GainKeyword Surge]

instance HasAbilities AnduinTrollSpawn where
  getAbilities (AnduinTrollSpawn a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After CombatPhase]

-- | Forced: At the beginning of the combat phase, Anduin Troll Spawn engages a player
-- engaged with a Hill Troll.
instance RunMessage AnduinTrollSpawn where
  runMessage msg e@(AnduinTrollSpawn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      trolls <- traverse getEnemy =<< select hillTrolls
      let players = nub [p | EngagedWith p <- map (.placement) trolls]
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "anduinTrollSpawn.engage"
        causedBy attrs
        targets players \p -> engageEnemy p attrs.id
      pure e
    _ -> AnduinTrollSpawn <$> runMessage msg attrs
