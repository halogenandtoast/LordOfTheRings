module LordOfTheRings.Enemy.Cards.NurnSlaver (nurnSlaver) where

import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (campsInStaging)
import LordOfTheRings.Trait (Trait (Camp))

newtype NurnSlaver = NurnSlaver EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

nurnSlaver :: EnemyCard NurnSlaver
nurnSlaver = enemy NurnSlaver Cards.nurnSlaver

-- | While the number of Camp locations in the staging area is less than the number of
-- players, Nurn Slaver gains surge.
instance HasModifiersFor NurnSlaver where
  getModifiersFor (NurnSlaver a) = do
    camps <- campsInStaging
    players <- length <$> getRemainingPlayers
    when (camps < players) $ modifySelf a [GainKeyword Surge]

-- | Shadow: Attacking enemy gets +1 Attack for each Camp location in the victory display.
instance RunMessage NurnSlaver where
  runMessage msg e@(NurnSlaver attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      camps <- getsGame (length . filter ((Camp `member`) . cdCardTraits . toCardDef) . (.victoryDisplay) . toAttrs . (.scenario))
      when (camps > 0) $ enemyGetsAttack (CardIdSource cid) eid camps
      pure e
    _ -> NurnSlaver <$> runMessage msg attrs
