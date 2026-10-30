module LordOfTheRings.Enemy.Cards.EyeOfTheBeast (eyeOfTheBeast) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype EyeOfTheBeast = EyeOfTheBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

eyeOfTheBeast :: EnemyCard EyeOfTheBeast
eyeOfTheBeast = enemy EyeOfTheBeast Cards.eyeOfTheBeast

-- | Eye of the Beast cannot attack.
-- While Eye of the Beast is engaged with a player, The Thing in the Depths attacks that
-- player during the combat phase (deal and resolve a shadow card for this attack): it
-- counts as engaged with him from the staging area, as Durin's Bane is with each player,
-- and is dealt a shadow card for the attack.
instance HasModifiersFor EyeOfTheBeast where
  getModifiersFor (EyeOfTheBeast a) = do
    modifySelf a [CannotAttack]
    for_ [pid | EngagedWith pid <- [a.placement]] \pid -> do
      things <- select (EnemyWithCardCode $ toCardCode Cards.theThingInTheDepths)
      for_ things \thing -> modified a thing [EngagedWithEachPlayer (PlayerWithId pid)]

instance RunMessage EyeOfTheBeast where
  runMessage msg (EyeOfTheBeast attrs) = EyeOfTheBeast <$> runMessage msg attrs
