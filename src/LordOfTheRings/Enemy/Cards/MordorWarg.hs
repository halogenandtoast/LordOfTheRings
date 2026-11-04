module LordOfTheRings.Enemy.Cards.MordorWarg (mordorWarg) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Orc))

newtype MordorWarg = MordorWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mordorWarg :: EnemyCard MordorWarg
mordorWarg = enemy MordorWarg Cards.mordorWarg

-- | Counts as a Mount attachment with the text: "Attached enemy gets +2 threat, +2 attack,
-- +2 defense and +2 hit points."
instance HasModifiersFor MordorWarg where
  getModifiersFor (MordorWarg a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> modified a host [ThreatModifier 2, AttackModifier 2, DefenseModifier 2, HitPointModifier 2]
    _ -> pure ()

{- | When Revealed: Attach to an Orc enemy and return that enemy to the staging area. Limit 1
per enemy: an Orc with a Mordor Warg already is not chosen. With no Orc to take it, it stays
in the staging area, an enemy.

Forced: When attached enemy leaves play, add Mordor Warg to the staging area: the game
does it for any enemy attached to an enemy as a Mount (see 'attachedEnemies').
-}
instance RunMessage MordorWarg where
  runMessage msg e@(MordorWarg attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      mounted <- getsGame \g -> [host | w <- toList g.entities.enemies, toCardCode w == toCardCode attrs, AttachedTo host <- [w.placement]]
      orcs <- filter ((`notElem` mounted) . toTarget) <$> select (EnemyWithTrait Orc)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "mordorWarg.attach"
        causedBy attrs
        targets orcs \orc -> do
          placeEnemy attrs.id (AttachedTo $ toTarget orc)
          placeEnemy orc StagingArea
      pure e
    _ -> MordorWarg <$> runMessage msg attrs
