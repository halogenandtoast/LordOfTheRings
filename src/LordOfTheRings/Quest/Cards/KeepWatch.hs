module LordOfTheRings.Quest.Cards.KeepWatch (keepWatch) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype KeepWatch = KeepWatch QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Limit 1 copy of Keep Watch in the victory display, on its card def.
keepWatch :: QuestCard KeepWatch
keepWatch = quest KeepWatch Cards.keepWatch

-- | While Keep Watch is in the victory display, each non-unique enemy engaged with a
-- player gets -1 attack.
instance HasModifiersFor KeepWatch where
  getModifiersFor (KeepWatch a) = whenM (inVictoryDisplay a.cardId) do
    enemies <- select (EnemyEngagedWith AnyPlayer <> not_ UniqueEnemy)
    for_ enemies \e -> modified a e [AttackModifier (-1)]
