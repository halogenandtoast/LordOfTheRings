module LordOfTheRings.Quest.Cards.FateOfTheDreamChaser (fateOfTheDreamChaser) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfCorsairs.Helpers (switchEncounterDeck)
import LordOfTheRings.Trait (Trait (Ocean, Ship))

newtype FateOfTheDreamChaser = FateOfTheDreamChaser QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fateOfTheDreamChaser :: QuestCard FateOfTheDreamChaser
fateOfTheDreamChaser = quest FateOfTheDreamChaser Cards.fateOfTheDreamChaser

{- | When Revealed: Each player assigns damage among non-Ship characters he controls equal
to the total damage on each Ship objective he controls. Discard all Ship enemies, Ship
objectives, and Ocean locations in play.
When Revealed (side B): Shuffle the Corsair deck and Corsair discard pile into the second
encounter deck. Search the encounter deck and discard pile for each copy of
Battle-hardened and each copy of Watch Tower, and shuffle them into the second encounter
deck. Set the current encounter deck and discard pile aside, inactive. The second
encounter deck becomes the active encounter deck. Advance to stage 3.
-}
instance RunMessage FateOfTheDreamChaser where
  runMessage msg q@(FateOfTheDreamChaser attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      FateOfTheDreamChaser <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        ships <- select (shipObjective <> AllyControlledBy (PlayerWithId p))
        damage <- sum <$> traverse (getDamage . toTarget) ships
        others <- selectCharacters (HeroControlledBy $ PlayerWithId p) (AllyControlledBy (PlayerWithId p) <> not_ (AllyWithTrait Ship))
        when (damage > 0 && notNull others) $ dealDamageDividedAmong attrs p others damage
      traverse_ (discard attrs) =<< select shipEnemy
      traverse_ (discard attrs) =<< select shipObjective
      traverse_ (discard attrs) =<< select (LocationWithTrait Ocean)
      switchEncounterDeck
      completeQuest attrs.id
      pure q
    _ -> FateOfTheDreamChaser <$> runMessage msg attrs
