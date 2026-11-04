module LordOfTheRings.Quest.Cards.FindTheOthers (findTheOthers) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor.Helpers (stagingHeroRules, takeHeroFromStagingArea)

newtype FindTheOthers = FindTheOthers QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

findTheOthers :: QuestCard FindTheOthers
findTheOthers = quest FindTheOthers Cards.findTheOthers

-- | Damage from undefended attacks must be dealt to a hero in the staging area, if able.
-- The first card each player plays each phase does not require a resource match.
instance HasModifiersFor FindTheOthers where
  getModifiersFor (FindTheOthers a) = when a.flipped $ stagingHeroRules a

{- | Forced: When this stage is defeated, each player chooses a hero he owns in the staging
area, takes control of that hero, and exhausts it.

The game's own pushes for a defeated stage go ahead of the stage's, which would put stage
3A's When Revealed before the heroes are taken: the stage, given its last progress, takes
them and then is defeated itself.
-}
instance RunMessage FindTheOthers where
  runMessage msg (FindTheOthers attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      let attrs' = attrs & tokensL %~ addTokens Progress n
      done <- hasEnoughProgress attrs'
      if done
        then do
          eachPlayer $ takeHeroFromStagingArea attrs
          completeQuest attrs.id
          pure $ FindTheOthers attrs'
        else FindTheOthers <$> runMessage msg attrs
    _ -> FindTheOthers <$> runMessage msg attrs
