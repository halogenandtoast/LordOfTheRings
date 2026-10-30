module LordOfTheRings.Enemy.Cards.GiantArm (giantArm) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GiantArm = GiantArm EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantArm :: EnemyCard GiantArm
giantArm = enemy GiantArm Cards.giantArm

-- | Giant Arm gets +2 attack when attacking allies: for an attack an ally defends, known
-- once its defenders are declared, before its shadow cards resolve.
-- Shadow: After this attack, either put Giant Arm into play engaged with you and deal it a
-- shadow card, or resolve its Grapple ability. The shadow card leaves the attacking enemy
-- as it resolves, to wait for the attack to end.
instance RunMessage GiantArm where
  runMessage msg e@(GiantArm attrs) = case msg of
    ResolveShadow eid _ defenders | eid == attrs.id -> do
      when (any (\case AllyTarget _ -> True; _ -> False) defenders)
        $ attackModifier attrs attrs (AttackModifier 2)
      GiantArm <$> runMessage msg attrs
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      let card = MkEncounterCard cid (toCardCode attrs)
      removeShadowCard eid cid
      choice <- capture $ chooseOneM pid do
        questionLabeled "giantArm.choose"
        labeled "giantArm.engage" do
          arm <- createEnemyEngaged pid card
          dealShadowCard arm
        labeled "giantArm.grapple" $ grappleCard card
      afterAttack eid choice
      pure e
    _ -> GiantArm <$> runMessage msg attrs
