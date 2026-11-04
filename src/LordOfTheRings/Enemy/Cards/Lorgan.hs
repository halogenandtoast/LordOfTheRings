module LordOfTheRings.Enemy.Cards.Lorgan (lorgan) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Lorgan = Lorgan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

lorgan :: EnemyCard Lorgan
lorgan = enemy Lorgan Cards.lorgan

-- | Immune to player card effects.
-- Only the engaged player can declare attackers or defenders against Lorgan.
instance HasModifiersFor Lorgan where
  getModifiersFor (Lorgan a) = modifySelf a [ImmuneToPlayerCardEffects, OnlyEngagedPlayerFights]

-- | Forced: After Lorgan attacks, raise each player's threat by 1 (2 instead if this attack
-- destroys a character).
instance HasAbilities Lorgan where
  getAbilities (Lorgan a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage Lorgan where
  runMessage msg e@(Lorgan attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      destroyed <- attackDestroyedACharacter attrs.id
      eachPlayer \pid -> encounterRaisesThreat pid (if destroyed then 2 else 1)
      pure e
    _ -> Lorgan <$> runMessage msg attrs
