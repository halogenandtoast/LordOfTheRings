module LordOfTheRings.Quest.Cards.ProtectTheFlanks (protectTheFlanks) where

import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ProtectTheFlanks = ProtectTheFlanks QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Siege, as its card def says.
protectTheFlanks :: QuestCard ProtectTheFlanks
protectTheFlanks = quest ProtectTheFlanks Cards.protectTheFlanks

-- | Forced: At the end of the combat phase, assign X damage among characters in play, where
-- X is the total number of shadow cards dealt to enemies in the staging area.
instance HasAbilities ProtectTheFlanks where
  getAbilities (ProtectTheFlanks a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase]

-- | The first player assigns it.
instance RunMessage ProtectTheFlanks where
  runMessage msg q@(ProtectTheFlanks attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      staged <- traverse getEnemy =<< select EnemyInStagingArea
      let x = sum [length e.shadowCards | e <- staged, e.placement == StagingArea]
      characters <- selectCharacters AnyHero AnyAlly
      lead <- getLead
      when (x > 0) $ dealDamageDividedAmong attrs lead characters x
      pure q
    _ -> ProtectTheFlanks <$> runMessage msg attrs
