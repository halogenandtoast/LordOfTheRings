module LordOfTheRings.Location.Cards.GlowingFissure (glowingFissure) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GlowingFissure = GlowingFissure LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

glowingFissure :: LocationCard GlowingFissure
glowingFissure = location GlowingFissure Cards.glowingFissure

instance HasModifiersFor GlowingFissure where
  getModifiersFor (GlowingFissure a) = do
    damaged <- damagedCharacters AnyHero AnyAlly
    modifySelf a [ThreatModifier $ 1 + length damaged]

instance HasAbilities GlowingFissure where
  getAbilities (GlowingFissure a) = [mkAbility a 1 $ forced $ AnyCommittedToQuestWindow After]

instance RunMessage GlowingFissure where
  runMessage msg l@(GlowingFissure a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      -- Committing a player's characters produces one batch of windows. The player
      -- takes this damage once for the batch, regardless of how many committed.
      players <- nub . catMaybes <$> traverse characterController [c | Window _ (CommittedToQuest c) <- ws]
      for_ players \pid -> do
        characters <- selectCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "glowingFissure.damage"
          causedBy a
          targets characters (\c -> dealDamage a c 1)
      pure l
    _ -> GlowingFissure <$> runMessage msg a
