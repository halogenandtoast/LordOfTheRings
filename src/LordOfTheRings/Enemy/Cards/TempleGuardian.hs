module LordOfTheRings.Enemy.Cards.TempleGuardian (templeGuardian) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Campaign (getCampaignLog)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Locations

newtype TempleGuardian = TempleGuardian EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

templeGuardian :: EnemyCard TempleGuardian
templeGuardian = enemy TempleGuardian Cards.templeGuardian

-- | X is the number of players in the game. Immune to player card effects. Cannot be
-- optionally engaged.
-- Temple Guardian gets -50 engagement cost while Cursed Temple is the active location.
-- The Dream-chaser campaign uses its Campaign Mode only side.
-- ponytail: that side is the printed side's modifiers, its card the same.
instance HasModifiersFor TempleGuardian where
  getModifiersFor (TempleGuardian a) = do
    players <- length <$> getRemainingPlayers
    cursed <- selectAny (IsActiveLocation <> LocationWithCardCode (toCardCode Locations.cursedTemple))
    campaign <- isJust <$> getCampaignLog
    modifySelf a
      $ [ThreatModifier players, ImmuneToPlayerCardEffects, CannotBeOptionallyEngagedBy AnyPlayer]
      <> [EngagementCostModifier (-50) | cursed]
      -- its Campaign Mode only side: 5 attack, defense X, archery X
      <> [m | campaign, m <- [AttackModifier (-1), DefenseModifier (players - 4), GainKeyword (Archery players)]]

instance RunMessage TempleGuardian where
  runMessage msg (TempleGuardian attrs) = TempleGuardian <$> runMessage msg attrs
