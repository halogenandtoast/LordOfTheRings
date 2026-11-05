module LordOfTheRings.Quest.Cards.ImminentPursuit (imminentPursuit) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype ImminentPursuit = ImminentPursuit QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

imminentPursuit :: QuestCard ImminentPursuit
imminentPursuit = quest ImminentPursuit Cards.imminentPursuit

-- | Forced: After characters are committed to the quest, each player must deal 2 damage to
-- a questing character he controls.
instance HasAbilities ImminentPursuit where
  getAbilities (ImminentPursuit a) =
    exploringGundabad a <> bypass a <> [mkAbility a 1 $ forced StagingStepBeginsWindow | a.flipped]

-- | Forced: When this stage is defeated, discard a non-unique enemy from play.
instance RunMessage ImminentPursuit where
  runMessage msg q@(ImminentPursuit attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> do
        questing <-
          selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId p)) (CommittedAlly <> AllyControlledBy (PlayerWithId p))
        chooseOrRunOneM p do
          questionLabeled "imminentPursuit.damage"
          causedBy attrs
          targets questing \c -> dealDamage attrs c 2
      pure q
    _ -> ImminentPursuit <$> runExploringGundabad defeated msg attrs
   where
    defeated = do
      enemies <- select (NotEnemy UniqueEnemy)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "imminentPursuit.discard"
        causedBy attrs
        targets enemies (discard attrs)
