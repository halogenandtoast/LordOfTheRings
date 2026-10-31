module LordOfTheRings.Enemy.Cards.RaidingParty (raidingParty) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ambush))

newtype RaidingParty = RaidingParty EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

raidingParty :: EnemyCard RaidingParty
raidingParty = enemy RaidingParty Cards.raidingParty

-- | Forced: After Raiding Party engages a player, each player discards the top card of the
-- encounter deck, starting with the first player. Each player who discards an enemy with
-- the ambush keyword must make an engagement check against that enemy.
instance HasAbilities RaidingParty where
  getAbilities (RaidingParty a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage RaidingParty where
  runMessage msg e@(RaidingParty attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let discarded = zip players deck
      discardTopOfEncounterDeck attrs (length discarded)
      -- ponytail: the engagement check is against that enemy alone: it comes out of the
      -- discard pile engaged with him if his threat reaches its printed engagement cost,
      -- else it stays discarded
      for_ discarded \(pid, card) -> do
        let def = toCardDef card
        threat <- getThreat pid
        when (cdCardType def == EnemyType && Ambush `member` cdKeywords def && maybe False (<= threat) (cdEngagementCost def)) do
          removeFromEncounterDiscard card
          putEnemyIntoPlayEngaged pid card
      pure e
    _ -> RaidingParty <$> runMessage msg attrs
