module LordOfTheRings.Attachment.Cards.GreatYewBow (greatYewBow) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype GreatYewBow = GreatYewBow AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero with the printed Ranged keyword. Restricted, on its card def.
greatYewBow :: AttachmentCard GreatYewBow
greatYewBow = attachment GreatYewBow Cards.greatYewBow & attachTo (AttachToHero $ HeroWithPrintedKeyword Ranged)

-- | Combat Action: Choose an enemy in the staging area. Exhaust Great Yew Bow and attached
-- hero to make a ranged attack against that enemy. Declare attached hero as the attacker. No
-- other attackers can be declared for this attack.
instance HasAbilities GreatYewBow where
  getAbilities (GreatYewBow a) =
    [ restricted a 1 (DuringPhase CombatPhase <> exists targetable) $ ActionAbility (Costs [exhaustCost a, ExhaustCost host])
    | AttachedTo host <- [a.placement]
    ]

targetable :: EnemyMatcher
targetable = EnemyInStagingArea <> EnemyNotImmuneToPlayerCards

instance RunMessage GreatYewBow where
  runMessage msg s@(GreatYewBow a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select targetable
      withHost a \host -> chooseOrRunOneM pid do
        questionLabeled "chooseEnemyToAttack"
        causedBy a
        targets enemies (declareSoleAttacker pid host)
      pure s
    _ -> GreatYewBow <$> runMessage msg a
