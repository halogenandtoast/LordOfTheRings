module LordOfTheRings.Quest.Cards.FollowTheOrcs (followTheOrcs) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (getPursuit, increasePursuit, setPursuit)
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype FollowTheOrcs = FollowTheOrcs QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

followTheOrcs :: QuestCard FollowTheOrcs
followTheOrcs = quest FollowTheOrcs Cards.followTheOrcs

{- | X is the pursuit value (1 printed, see 'Cards.followTheOrcs').
Uglúk cannot leave the staging area or take damage.
-}
instance HasModifiersFor FollowTheOrcs where
  getModifiersFor (FollowTheOrcs a) = do
    pursuit <- getPursuit
    modifySelf a [QuestPointModifier (pursuit - 1)]
    when a.flipped do
      ugluk <- select (EnemyWithCardCode $ toCardCode Enemies.ugluk)
      for_ ugluk \e -> modified a e [CannotLeaveStagingArea, CannotBeOptionallyEngagedBy AnyPlayer, CannotTakeDamage]

-- | Forced: At the end of the combat phase, increase the pursuit value by 1 for each enemy
-- in the staging area.
instance HasAbilities FollowTheOrcs where
  getAbilities (FollowTheOrcs a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase | a.flipped]

{- | Setup: Make Eastemnet the active location. Add Uglúk and To Isengard! to the staging
area. Each player chooses 1 non-Fellowship hero he controls to be a Captive and places it
faceup underneath To Isengard! Set the pursuit value to 10. (The scenario sets the three
cards aside as it gathers.)

The Captives are placed before To Isengard! enters play, so it is never in play without
them.
-}
instance RunMessage FollowTheOrcs where
  runMessage msg q@(FollowTheOrcs attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      aside <- getSetAside AnyCard
      let named def = filter (cardMatches $ cardIs def) aside
      traverse_ putIntoPlayAsActiveLocation (named Locations.eastemnet)
      for_ (named Objectives.toIsengard) \toIsengard -> do
        eachPlayer \p -> do
          heroes <- select (HeroControlledBy (PlayerWithId p) <> not_ (HeroWithSphere Fellowship))
          chooseOrRunOneM p do
            questionLabeled "followTheOrcs.captive"
            causedBy attrs
            targets heroes \h -> guardCharacterWith h toIsengard
        addSetAsideToStaging (cardIs Objectives.toIsengard)
      addSetAsideToStaging (cardIs Enemies.ugluk)
      setPursuit 10
      FollowTheOrcs <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      enemies <- length <$> select EnemyInStagingArea
      when (enemies > 0) $ increasePursuit enemies
      pure q
    _ -> FollowTheOrcs <$> runMessage msg attrs
