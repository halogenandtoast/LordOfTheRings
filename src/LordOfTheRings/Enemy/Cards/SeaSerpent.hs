module LordOfTheRings.Enemy.Cards.SeaSerpent (seaSerpent, seaSerpentEnraged) where

import LordOfTheRings.Card (genEncounterCard, toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

-- | A burden of the Dream-chaser campaign, its Menacing side, which turns Enraged as it is
-- destroyed.
newtype SeaSerpent = SeaSerpent EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

seaSerpent :: EnemyCard SeaSerpent
seaSerpent = enemy SeaSerpent Cards.seaSerpent

seaSerpentEnraged :: EnemyCard SeaSerpent
seaSerpentEnraged = enemy SeaSerpent Cards.seaSerpentEnraged

enraged :: EnemyAttrs -> Bool
enraged a = toCardCode a == toCardCode Cards.seaSerpentEnraged

-- | Immune to player card effects. Menacing: X is the number of players in the game.
-- Enraged: X is twice the number of players. Its threat and defense are X.
instance HasModifiersFor SeaSerpent where
  getModifiersFor (SeaSerpent a) = do
    x <- xOf a
    modifySelf a $ [ImmuneToPlayerCardEffects, ThreatModifier x, DefenseModifier x] <> [StaysInPlayWhenDefeated | not (enraged a)]

xOf :: HasGame m => EnemyAttrs -> m Int
xOf a = (* (if enraged a then 2 else 1)) . length <$> getRemainingPlayers

{- | Forced: After Sea Serpent attacks and destroys an ally, the defending player deals X
damage divided among characters they control.
Menacing: Forced: After Sea Serpent is destroyed, flip it over and put it into play in the
staging area.
Enraged: Forced: At the end of the round, Sea Serpent makes an attack against the first
player.
-}
instance RunMessage SeaSerpent where
  runMessage msg (SeaSerpent attrs) = case msg of
    CheckWindows ws -> do
      x <- xOf attrs
      for_ [pid | Window After (CharacterDestroyedByAttack eid (AllyTarget _) pid) <- ws, eid == attrs.id] \pid ->
        dealDamageDivided attrs pid x
      when (not (enraged attrs) && or [eid == attrs.id | Window After (EnemyDefeated eid) <- ws]) do
        removeFromGame attrs
        addToStaging =<< genEncounterCard Cards.seaSerpentEnraged
      when (enraged attrs && Window When RoundEnds `elem` ws) do
        lead <- getLead
        immediateAttack lead attrs.id
      SeaSerpent <$> runMessage msg attrs
    _ -> SeaSerpent <$> runMessage msg attrs
