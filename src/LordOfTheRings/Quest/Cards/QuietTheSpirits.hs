module LordOfTheRings.Quest.Cards.QuietTheSpirits (quietTheSpirits) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (flipToClue)
import LordOfTheRings.Trait (Trait (Undead))

newtype QuietTheSpirits = QuietTheSpirits QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
quietTheSpirits :: QuestCard QuietTheSpirits
quietTheSpirits = quest QuietTheSpirits Cards.quietTheSpirits

-- | While Quiet the Spirits is the current quest, each Undead enemy that enters play
-- immediately engages the player with the highest threat. Enemies that engage a player
-- from this effect contribute their threat to the total threat in the staging area until
-- the end of the phase.
-- Forced: After an enemy is destroyed, place 2 progress on Quiet the Spirits.
-- ponytail: an enemy enters play in the staging area here; one put into play engaged is
-- engaged already.
instance HasAbilities QuietTheSpirits where
  getAbilities (QuietTheSpirits a) =
    [ restricted a 1 (TargetHasModifier (toTarget a) CurrentQuest) $ forced $ EnemyEntersStagingAreaWindow After
    , mkAbility a 2 $ forced $ EnemyDefeatedWindow After
    ]

-- | When this quest is defeated, flip it over.
instance RunMessage QuietTheSpirits where
  runMessage msg q@(QuietTheSpirits attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      undead <- filterM (\e -> selectAny (EnemyWithId e <> EnemyWithTrait Undead)) [e | Window _ (EntersStagingArea (EnemyTarget e)) <- windows]
      players <- getRemainingPlayers
      threats <- for players \p -> (p,) . (.threat) <$> getPlayer p
      let highest = foldr (max . snd) 0 threats
          tied = [p | (p, t) <- threats, t == highest]
      lead <- getLead
      for_ undead \e -> do
        chooseOrRunOneM lead do
          questionLabeled "quietTheSpirits.engage"
          causedBy attrs
          targets tied \p -> engageEnemy p e
        phaseModifier attrs GameTarget (EngagedEnemiesAddThreat $ EnemyWithId e)
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      placeProgress attrs attrs 2
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      flipToClue attrs Objectives.daechanarsBrand
      QuietTheSpirits <$> runMessage msg attrs
    _ -> QuietTheSpirits <$> runMessage msg attrs
