module LordOfTheRings.Enemy.Cards.ThaneUlchorTheTempleOfDoom (thaneUlchorTheTempleOfDoom) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom.Helpers (placeDamageOnTribute, templeX)

newtype ThaneUlchor = ThaneUlchor EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

thaneUlchorTheTempleOfDoom :: EnemyCard ThaneUlchor
thaneUlchorTheTempleOfDoom = enemy ThaneUlchor Cards.thaneUlchorTheTempleOfDoom

{- | Indestructible. Immune to player card effects.
X is 1 more than the number of quest stages in the victory display. Its threat is X.
Cannot be engaged unless X is equal to 5: neither by engagement check nor by choice.
-}
instance HasModifiersFor ThaneUlchor where
  getModifiersFor (ThaneUlchor a) = do
    x <- templeX
    modifySelf a [ImmuneToPlayerCardEffects, ThreatModifier x]
    unless (x == 5) do
      players <- getRemainingPlayers
      modifySelf a (map CannotEngagePlayer players)

-- | Forced: When Thane Ulchor attacks and destroys a character, place 1 damage token on
-- Tribute to Mordor. Read as the character is destroyed.
instance HasAbilities ThaneUlchor where
  getAbilities (ThaneUlchor a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

instance RunMessage ThaneUlchor where
  runMessage msg e@(ThaneUlchor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [() | Window _ (CharacterDestroyedByAttack {}) <- ws] \_ -> placeDamageOnTribute attrs
      pure e
    _ -> ThaneUlchor <$> runMessage msg attrs
