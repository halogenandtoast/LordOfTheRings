module LordOfTheRings.Enemy.Cards.DarkBats (darkBats) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition))

newtype DarkBats = DarkBats EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkBats :: EnemyCard DarkBats
darkBats = enemy DarkBats Cards.darkBats

-- | While attached, counts as a Condition attachment with the text: "Attached character
-- gets -1 willpower, -1 attack, and -1 defense." Attached, it has the Condition trait, for
-- what discards a Condition attachment to find.
instance HasModifiersFor DarkBats where
  getModifiersFor (DarkBats a) = withHost a \host -> do
    modified a host [WillpowerModifier (-1), AttackModifier (-1), DefenseModifier (-1)]
    modifySelf a [GainTrait Condition]

{- | Forced: After Dark Bats damages a character, attach it to that character. It attaches
once its attack has resolved, as Strangling Python does, if the character is still in
play.
Shadow: Attach this card to the defending character.
-}
instance RunMessage DarkBats where
  runMessage msg e@(DarkBats attrs) = case msg of
    DoStep 1 (DealDamage (sourceEnemy -> Just eid) target n) | eid == attrs.id, n > 0, isCharacter target -> do
      afterAttack attrs.id [DoStep 2 msg]
      DarkBats <$> runMessage msg attrs
    DoStep 2 (DoStep 1 (DealDamage (sourceEnemy -> Just eid) target _)) | eid == attrs.id -> do
      alive <- case target of
        HeroTarget hid -> selectAny (HeroWithId hid)
        AllyTarget aid -> selectAny (AllyWithId aid)
        _ -> pure False
      when (alive && not (attached attrs.placement)) $ placeEnemy attrs.id (AttachedTo target)
      pure e
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      chooseOrRunOneM pid do
        questionLabeled "darkBats.attach"
        causedBy attrs
        targets defenders $ attachShadowEnemy eid attrs.cardId
      pure e
    _ -> DarkBats <$> runMessage msg attrs
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
    attached = \case
      AttachedTo _ -> True
      _ -> False
