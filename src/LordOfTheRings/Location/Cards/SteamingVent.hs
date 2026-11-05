module LordOfTheRings.Location.Cards.SteamingVent (steamingVent) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (wizardry)

newtype SteamingVent = SteamingVent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

steamingVent :: LocationCard SteamingVent
steamingVent = location SteamingVent Cards.steamingVent

{- | While Steaming Vent is in the staging area, it gains: "Forced: After a player commits
characters to the quest, he deals 1 damage to a questing character he controls."
Wizardry: Assign X damage among characters in play. X is 1 more than the number of
players in the game (see 'wizardry').
-}
instance HasAbilities SteamingVent where
  getAbilities (SteamingVent a) =
    [mkAbility a 1 $ forced $ AnyCommittedToQuestWindow After | a.placement == StagingArea]
      <> [mkAbility a 2 $ forced $ ExploredWindow When a.id]

instance RunMessage SteamingVent where
  runMessage msg l@(SteamingVent a) = case msg of
    -- a player's characters are committed together, in one window
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      players <- nub . catMaybes <$> traverse characterController [c | Window _ (CommittedToQuest c) <- ws]
      for_ players \p -> do
        questing <- selectCharacters (HeroControlledBy (PlayerWithId p) <> CommittedHero) (AllyControlledBy (PlayerWithId p) <> CommittedAlly)
        chooseOrRunOneM p do
          questionLabeled "steamingVent.damage"
          causedBy a
          targets questing \c -> dealDamage a c 1
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      wizardry a (toCardCode a) =<< getRemainingPlayers
      pure l
    _ -> SteamingVent <$> runMessage msg a
