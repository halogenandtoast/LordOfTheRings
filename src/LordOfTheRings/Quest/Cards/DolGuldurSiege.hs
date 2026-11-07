module LordOfTheRings.Quest.Cards.DolGuldurSiege
  (theSiegeOfDolGuldur, theTowerOfSorcery, battleUnderTheTrees, dungeonsOfTheNecromancer) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Board (epicVictoryCopies)
import LordOfTheRings.Epic.Types (EpicEffect (EpicDolSearchCard, EpicDolSearchAlly, EpicDestroyEnemy), EpicTargetQuery (EpicNonUniqueEnemy))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers
import LordOfTheRings.Trait (Trait (Lieutenant))

newtype DolGuldurSiege = DolGuldurSiege QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theSiegeOfDolGuldur, theTowerOfSorcery, battleUnderTheTrees, dungeonsOfTheNecromancer :: QuestCard DolGuldurSiege
theSiegeOfDolGuldur = quest DolGuldurSiege Cards.theSiegeOfDolGuldur
theTowerOfSorcery = quest DolGuldurSiege Cards.theTowerOfSorcery
battleUnderTheTrees = quest DolGuldurSiege Cards.battleUnderTheTrees
dungeonsOfTheNecromancer = quest DolGuldurSiege Cards.dungeonsOfTheNecromancer

lieutenantFor :: QuestAttrs -> CardDef
lieutenantFor a
  | toCardCode a == toCardCode Cards.theSiegeOfDolGuldur = Enemies.baneOfAmonLanc
  | toCardCode a == toCardCode Cards.theTowerOfSorcery = Enemies.sorcererOfDolGuldurAttack
  | toCardCode a == toCardCode Cards.battleUnderTheTrees = Enemies.chieftainMorlug
  | otherwise = Enemies.dungeonDweller

instance HasModifiersFor DolGuldurSiege where
  getModifiersFor (DolGuldurSiege a) = when a.flipped do
    foes <- select $ EnemyWithCardCode $ toCardCode $ lieutenantFor a
    for_ foes $ \eid -> modified a eid [EngagementCostModifier $ negate a.progress]

instance HasAbilities DolGuldurSiege where
  getAbilities (DolGuldurSiege a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

instance RunMessage DolGuldurSiege where
  runMessage msg q@(DolGuldurSiege a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      DolGuldurSiege <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      lieutenants <- select $ EnemyWithTrait Lieutenant
      traverse_ (push . SetAsideEnemy) lieutenants
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget a -> True)) -> do
      -- A defeated Lieutenant is never recreated, including in Epic mode.
      victory <- epicVictoryCopies $ toCardCode $ lieutenantFor a
      when (victory == 0) $ addSetAsideToStaging $ cardIs $ lieutenantFor a
      doStep 2 $ WhenRevealed $ toTarget a
      pure q
    DoStep 2 (WhenRevealed (isTarget a -> True)) -> do
      strength <- cityStrength
      threshold <- bool 5 10 <$> epicMode
      when (strength <= threshold) do
        foes <- select $ EnemyWithCardCode $ toCardCode $ lieutenantFor a
        players <- getPlayerOrder
        for_ (headMay foes) $ \eid -> push $ EnemyAttacksPlayers eid players False []
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      if a.progress >= 5 then chooseOneM lead do
        questionLabeled "dolGuldur.preventPenalty"
        causedBy a
        labeled "dolGuldur.removeFive" $ removeTokens a a Progress 5
        labeled "dolGuldur.resolvePenalty" $ stagePenalty $ toCardCode a
      else stagePenalty $ toCardCode a
      doStep 1 msg
      doStep 3 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      when (a.progress >= 5) do
        lead <- getLead
        chooseOneM lead do
          questionLabeled "dolGuldur.stageBenefit"
          causedBy a
          labeled "dolGuldur.payFive" do
            removeTokens a a Progress 5
            push $ DoStep 2 $ UseCardAbility lead (toSource a) 1 []
          labeled "skip" $ pure ()
      pure q
    DoStep 2 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      lead <- getLead
      if | toCardCode a == toCardCode Cards.theSiegeOfDolGuldur -> damageDolGuldur a 3
         | toCardCode a == toCardCode Cards.theTowerOfSorcery -> chooseDolGuldurPlayer msg lead a EpicDolSearchCard
         | toCardCode a == toCardCode Cards.battleUnderTheTrees -> chooseEpicTarget a lead EpicNonUniqueEnemy EpicDestroyEnemy
         | otherwise -> chooseDolGuldurPlayer msg lead a EpicDolSearchAlly
      pure q
    DoStep 3 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      push $ ScenarioSpecific "dolGuldur.stageForcedResolved" $ toJSON $ toCardCode a
      pure q
    _ -> DolGuldurSiege <$> runMessage msg a
