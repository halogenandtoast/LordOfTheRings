module LordOfTheRings.Enemy.Cards.TempleGuardian (templeGuardian, templeGuardianCampaign) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Locations

newtype TempleGuardian = TempleGuardian EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

templeGuardian :: EnemyCard TempleGuardian
templeGuardian = enemy TempleGuardian Cards.templeGuardian

-- | Its Campaign Mode only side, which the Dream-chaser campaign plays.
templeGuardianCampaign :: EnemyCard TempleGuardian
templeGuardianCampaign = enemy TempleGuardian Cards.templeGuardianCampaign

-- | X is the number of players in the game. Immune to player card effects. Cannot be
-- optionally engaged.
-- Temple Guardian gets -50 engagement cost while Cursed Temple is the active location.
-- Its Campaign Mode only side has defense X and archery X too.
instance HasModifiersFor TempleGuardian where
  getModifiersFor (TempleGuardian a) = do
    players <- length <$> getRemainingPlayers
    cursed <- selectAny (IsActiveLocation <> LocationWithCardCode (toCardCode Locations.cursedTemple))
    let campaign = toCardCode a == toCardCode Cards.templeGuardianCampaign
    modifySelf a
      $ [ThreatModifier players, ImmuneToPlayerCardEffects, CannotBeOptionallyEngagedBy AnyPlayer]
      <> [EngagementCostModifier (-50) | cursed]
      <> [m | campaign, m <- [DefenseModifier players, GainKeyword (Archery players)]]

instance RunMessage TempleGuardian where
  runMessage msg (TempleGuardian attrs) = TempleGuardian <$> runMessage msg attrs
