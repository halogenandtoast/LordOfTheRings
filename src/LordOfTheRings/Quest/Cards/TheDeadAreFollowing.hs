module LordOfTheRings.Quest.Cards.TheDeadAreFollowing (theDeadAreFollowing) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Phantom))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (overcomeByFearOn)
import LordOfTheRings.Trait (Trait (Oathbreaker))

newtype TheDeadAreFollowing = TheDeadAreFollowing QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theDeadAreFollowing :: QuestCard TheDeadAreFollowing
theDeadAreFollowing = quest TheDeadAreFollowing Cards.theDeadAreFollowing

-- | While Stone of Erech is the active location, each Oathbreaker enemy loses the Phantom
-- keyword and gets -20 engagement cost. The players do not win by defeating this stage:
-- its progress piles up for The Stone of Erech's travel cost.
instance HasModifiersFor TheDeadAreFollowing where
  getModifiersFor (TheDeadAreFollowing a) = when a.flipped do
    modifySelf a [CannotBeDefeated]
    stone <- selectAny (IsActiveLocation <> LocationWithCardCode (toCardCode Locations.theStoneOfErech))
    when stone do
      oathbreakers <- select (EnemyWithTrait Oathbreaker)
      for_ oathbreakers \e -> modified a e [LoseKeyword Phantom, EngagementCostModifier (-20)]

-- | When The Stone of Erech leaves play as an explored location, the players win the game.
instance HasAbilities TheDeadAreFollowing where
  getAbilities (TheDeadAreFollowing a) =
    [mkAbility a 1 $ forced $ ExploredMatchingWindow After (CardWithCode $ toCardCode Locations.theStoneOfErech) | a.flipped]

-- | When Revealed: Add The Stone of Erech and Army of the Dead (enemy side faceup) to the
-- staging area. Each player may raise his threat by 3 to detach Overcome by Fear from his
-- threat dial and set it aside, out of play.
instance RunMessage TheDeadAreFollowing where
  runMessage msg q@(TheDeadAreFollowing attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheDeadAreFollowing <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (CardWithCode $ toCardCode Locations.theStoneOfErech)
      addSetAsideToStaging (CardWithCode $ toCardCode Enemies.armyOfTheDead)
      eachPlayer \p -> do
        fears <- overcomeByFearOn p
        for_ fears \o -> chooseOneM p do
          questionLabeled "theDeadAreFollowing.choose"
          causedBy attrs
          labeled "theDeadAreFollowing.detach" do
            encounterRaisesThreat p 3
            setAsideObjective o
          labeled "pass" $ pure ()
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      winGame
      pure q
    _ -> TheDeadAreFollowing <$> runMessage msg attrs
