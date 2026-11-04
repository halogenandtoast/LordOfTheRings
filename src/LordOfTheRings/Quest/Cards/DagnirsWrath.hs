module LordOfTheRings.Quest.Cards.DagnirsWrath (dagnirsWrath) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype DagnirsWrath = DagnirsWrath QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dagnirsWrath :: QuestCard DagnirsWrath
dagnirsWrath = quest DagnirsWrath Cards.dagnirsWrath

-- | Forced: After characters are committed to the quest, Dagnir makes an immediate attack
-- against each player with 6 or more characters.
instance HasAbilities DagnirsWrath where
  getAbilities (DagnirsWrath a) =
    exploringGundabad a <> bypass a <> [mkAbility a 1 $ forced StagingStepBeginsWindow | a.flipped]

-- | Forced: When this stage is defeated, deal 5 damage to Dagnir. This damage is not
-- limited by Dagnir's text, and does not count toward her X value: Dagnir takes the damage
-- of this stage past her limit (see her card).
instance RunMessage DagnirsWrath where
  runMessage msg q@(DagnirsWrath attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      getDagnir >>= traverse_ \dagnir -> eachPlayer \p -> do
        characters <- charactersControlledBy p
        when (length characters >= 6) $ immediateAttack p dagnir
      pure q
    _ -> DagnirsWrath <$> runExploringGundabad defeated msg attrs
   where
    defeated = getDagnir >>= traverse_ \dagnir -> dealDamage attrs dagnir 5
