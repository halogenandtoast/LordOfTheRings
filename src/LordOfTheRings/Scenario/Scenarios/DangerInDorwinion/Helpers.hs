-- | What the cards of Danger in Dorwinion share: the set-aside Plot objectives and Cultist
-- enemies, and the resources the main quest counts.
module LordOfTheRings.Scenario.Scenarios.DangerInDorwinion.Helpers (
  cultist,
  isPlotOrCultist,
  addRandomSetAside,
  questResources,
) where

import LordOfTheRings.Card (CardType (EnemyType, ObjectiveType), EncounterCard, toCardCode)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (QuestAttrs (questTokens))
import LordOfTheRings.Token (Token (Resource), countTokens)
import LordOfTheRings.Trait (Trait (Cultist, Plot))

-- | "The Cultist": the Cultist enemy Leader of the Cult adds.
cultist :: EnemyMatcher
cultist = EnemyWithTrait Cultist

-- | Stage 1A sets each Plot objective and each Cultist enemy aside, out of play.
isPlotOrCultist :: EncounterCard -> Bool
isPlotOrCultist =
  cardMatches
    $ CardMatchAny
      [ CardMatchAll [CardWithType ObjectiveType, CardWithTrait Plot]
      , CardMatchAll [CardWithType EnemyType, CardWithTrait Cultist]
      ]

-- | Add 1 random set-aside card of the trait, a Plot objective or a Cultist enemy, to the
-- staging area.
addRandomSetAside :: ReverseQueue m => Trait -> m ()
addRandomSetAside trait = do
  aside <- getSetAside (CardWithTrait trait)
  unless (null aside) do
    card <- uniform aside
    addSetAsideToStaging (CardWithCode $ toCardCode card)

questResources :: QuestAttrs -> Int
questResources = countTokens Resource . questTokens
