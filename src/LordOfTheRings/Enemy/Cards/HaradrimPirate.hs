module LordOfTheRings.Enemy.Cards.HaradrimPirate (haradrimPirate) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (shipIsActive)

newtype HaradrimPirate = HaradrimPirate EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

haradrimPirate :: EnemyCard HaradrimPirate
haradrimPirate = enemy HaradrimPirate Cards.haradrimPirate

-- | Forced: When Haradrim Pirate engages you, deal 1 damage to a character you control (2
-- damage instead if the active location is a Ship).
instance HasAbilities HaradrimPirate where
  getAbilities (HaradrimPirate a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

-- | Shadow: Defending character gets -1 defense for each damage on it.
instance RunMessage HaradrimPirate where
  runMessage msg e@(HaradrimPirate attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      ship <- shipIsActive
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> dealDamageToOneCharacter attrs pid (if ship then 2 else 1)
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> do
        n <- getDamage d
        when (n > 0) $ attackModifier (CardIdSource cid) d (DefenseModifier (negate n))
      pure e
    _ -> HaradrimPirate <$> runMessage msg attrs
