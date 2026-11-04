module LordOfTheRings.Enemy.Cards.RacingWarg (racingWarg) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Orc))

newtype RacingWarg = RacingWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge.
racingWarg :: EnemyCard RacingWarg
racingWarg = enemy RacingWarg Cards.racingWarg

-- | Counts as a Mount attachment with the text: "Attached enemy gets +1 threat, +1 attack,
-- +1 defense, and +1 hit point."
instance HasModifiersFor RacingWarg where
  getModifiersFor (RacingWarg a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> modified a host [ThreatModifier 1, AttackModifier 1, DefenseModifier 1, HitPointModifier 1]
    _ -> pure ()

{- | When Revealed: Attach to an Orc enemy. Limit 1 per enemy: an Orc with a Racing Warg
already is not chosen. With no Orc to take it, it stays in the staging area, an enemy. An
Orc in the Orcs' area may take it, as an encounter card effect.

Forced: When attached enemy leaves play, return Racing Warg to the staging area: the game
does it for any enemy attached to an enemy as a Mount, as Mordor Warg's.
-}
instance RunMessage RacingWarg where
  runMessage msg e@(RacingWarg attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      mounted <- getsGame \g -> [host | w <- toList g.entities.enemies, toCardCode w == toCardCode attrs, AttachedTo host <- [w.placement]]
      orcs <- filter ((`notElem` mounted) . toTarget) <$> select (EnemyWithTrait Orc)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "racingWarg.attach"
        causedBy attrs
        targets orcs \orc -> placeEnemy attrs.id (AttachedTo $ toTarget orc)
      pure e
    _ -> RacingWarg <$> runMessage msg attrs
