module LordOfTheRings.Quest.Cards.StormTheCastleLethalCounterattack (stormTheCastleLethalCounterattack) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (runStormTheCastle)

newtype StormTheCastleLethalCounterattack = StormTheCastleLethalCounterattack QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest: Storm the Castle, Lethal Counterattack on its other side.
stormTheCastleLethalCounterattack :: QuestCard StormTheCastleLethalCounterattack
stormTheCastleLethalCounterattack = quest StormTheCastleLethalCounterattack Cards.stormTheCastleLethalCounterattack

-- | When Revealed: Deal 1 damage to each character in play.
instance RunMessage StormTheCastleLethalCounterattack where
  runMessage msg q@(StormTheCastleLethalCounterattack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> dealDamage attrs c 1
      pure q
    _ -> StormTheCastleLethalCounterattack <$> runStormTheCastle msg attrs
