module LordOfTheRings.Ally.Cards.AzainSilverbeard (azainSilverbeard) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype AzainSilverbeard = AzainSilverbeard AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

azainSilverbeard :: AllyCard AzainSilverbeard
azainSilverbeard = ally AzainSilverbeard Cards.azainSilverbeard

-- | Response: After Azain Silverbeard participates in an attack that destroys an enemy,
-- spend 1 tactics resource to deal 2 damage to another enemy that shares a Trait with the
-- destroyed enemy. Any player may trigger this response.
instance HasAbilities AzainSilverbeard where
  getAbilities (AzainSilverbeard a) =
    [ anyPlayer
        $ mkAbility a 1
        $ ResponseAbility (EnemyDestroyedByAttackWindow After (Just $ toTarget a)) (SpendResourcesCost Tactics 1)
    ]

instance RunMessage AzainSilverbeard where
  runMessage msg s@(AzainSilverbeard a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ (take 1 [(eid, traits) | Window _ (EnemyDestroyedByAttack eid _ traits) <- windows]) \(eid, traits) -> do
        sharing <- ordNub . concat <$> traverse (\t -> select (EnemyWithTrait t <> not_ (EnemyWithId eid))) (toList traits)
        chooseOrRunOneM pid do
          questionLabeled "azainSilverbeard.damage"
          causedBy a
          targets sharing \e -> dealDamage a e 2
      pure s
    _ -> AzainSilverbeard <$> runMessage msg a
