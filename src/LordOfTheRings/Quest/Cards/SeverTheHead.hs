module LordOfTheRings.Quest.Cards.SeverTheHead (severTheHead) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum.Helpers (getThaurdir)

newtype SeverTheHead = SeverTheHead QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Battle, as its card def says.
severTheHead :: QuestCard SeverTheHead
severTheHead = quest SeverTheHead Cards.severTheHead

-- | Thaurdir gets +2 threat, +2 attack, and cannot take damage.
instance HasModifiersFor SeverTheHead where
  getModifiersFor (SeverTheHead a) =
    getThaurdir >>= traverse_ \t -> modified a t [ThreatModifier 2, AttackModifier 2, CannotTakeDamage]

instance RunMessage SeverTheHead where
  runMessage msg (SeverTheHead attrs) = SeverTheHead <$> runMessage msg attrs
