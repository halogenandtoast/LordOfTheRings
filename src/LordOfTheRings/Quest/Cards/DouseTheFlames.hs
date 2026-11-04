module LordOfTheRings.Quest.Cards.DouseTheFlames (douseTheFlames) where

import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (damageHrogarsHill, getHrogarsHill)

newtype DouseTheFlames = DouseTheFlames QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
douseTheFlames :: QuestCard DouseTheFlames
douseTheFlames = quest DouseTheFlames Cards.douseTheFlames

-- | Forced: At the end of the quest phase, deal 5 damage to Hrogar's Hill.
instance HasAbilities DouseTheFlames where
  getAbilities (DouseTheFlames a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase]

-- | Response: When Douse the Flames is defeated, remove 10 damage from Hrogar's Hill. The
-- players gain by it, so it is taken, as Seek Shade's is.
instance RunMessage DouseTheFlames where
  runMessage msg q@(DouseTheFlames attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      damageHrogarsHill attrs 5
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      getHrogarsHill >>= traverse_ \hill -> do
        damage <- getDamage (QuestTarget hill)
        when (damage > 0) $ removeTokens attrs (QuestTarget hill) Damage (min 10 damage)
      DouseTheFlames <$> runMessage msg attrs
    _ -> DouseTheFlames <$> runMessage msg attrs
