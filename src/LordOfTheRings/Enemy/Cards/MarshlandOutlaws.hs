module LordOfTheRings.Enemy.Cards.MarshlandOutlaws (marshlandOutlaws) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype MarshlandOutlaws = MarshlandOutlaws EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

marshlandOutlaws :: EnemyCard MarshlandOutlaws
marshlandOutlaws = enemy MarshlandOutlaws Cards.marshlandOutlaws

-- | If you are engaged with this enemy, you cannot attack or deal damage (through effects)
-- to enemies with a title other than Marshland Outlaws.
instance HasModifiersFor MarshlandOutlaws where
  -- ponytail: only the attacks are barred; damage from card effects still lands
  getModifiersFor (MarshlandOutlaws a) = for_ [p | EngagedWith p <- [a.placement]] \pid -> do
    others <- select (NotEnemy $ EnemyWithCardCode $ toCardCode Cards.marshlandOutlaws)
    for_ others \e -> modified a e [CannotBeAttackedByCharactersOf pid]

-- | Shadow: Raise your threat by X. X is the amount of damage dealt by this attack.
instance RunMessage MarshlandOutlaws where
  runMessage msg e@(MarshlandOutlaws attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      attackModifier attrs eid (AttackDamageRaisesThreatOf pid)
      pure e
    _ -> MarshlandOutlaws <$> runMessage msg attrs
