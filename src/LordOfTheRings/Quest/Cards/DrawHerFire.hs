module LordOfTheRings.Quest.Cards.DrawHerFire (drawHerFire) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (dagnirAttacksFirstPlayer, getDagnir)

newtype DrawHerFire = DrawHerFire QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
drawHerFire :: QuestCard DrawHerFire
drawHerFire = quest DrawHerFire Cards.drawHerFire

-- | Shadow cards dealt to Dagnir are immune to player card effects and their effects
-- cannot be canceled.
instance HasModifiersFor DrawHerFire where
  getModifiersFor (DrawHerFire a) =
    getDagnir >>= traverse_ \d -> do
      shadows <- (.shadowCards) <$> getEnemy d
      for_ shadows \c -> modified a (CardIdTarget c.id) [ImmuneToPlayerCardEffects, ShadowCannotBeCanceled]

-- | Forced: After Draw Her Fire becomes the current quest, Dagnir makes an immediate attack
-- against the first player.
instance RunMessage DrawHerFire where
  runMessage msg (DrawHerFire attrs) = case msg of
    SetCurrentQuest (isTarget attrs -> True) -> do
      dagnirAttacksFirstPlayer
      DrawHerFire <$> runMessage msg attrs
    _ -> DrawHerFire <$> runMessage msg attrs
