-- | What the cards of Into Fangorn share: Mugash, wherever he is, and the Huorns.
module LordOfTheRings.Scenario.Scenarios.IntoFangorn.Helpers (
  huorn,
  mugashOnHero,
  mugashHost,
  canDefeatStage,
  shuffleMugashIntoEncounterDeck,
  searchForHuornEngaged,
  revealFromDiscard,
) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (Objective)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types ()
import LordOfTheRings.Target (Target (HeroTarget))
import LordOfTheRings.Trait (Trait (Huorn))

-- | A Huorn enemy card.
huorn :: CardMatcher
huorn = CardWithType EnemyType <> CardWithTrait Huorn

-- | Mugash, the objective, in play.
mugash :: HasGame m => m (Maybe Objective)
mugash = getsGame $ find ((== toCardCode Objectives.mugashObjective) . toCardCode) . toList . (.entities.objectives)

-- | Whether Mugash is attached to a hero.
mugashOnHero :: HasGame m => m Bool
mugashOnHero =
  mugash <&> \case
    Just o | AttachedTo (HeroTarget _) <- o.placement -> True
    _ -> False

-- | The hero Mugash is attached to.
mugashHost :: HasGame m => m (Maybe Target)
mugashHost =
  mugash <&> \case
    Just o | AttachedTo host@(HeroTarget _) <- o.placement -> Just host
    _ -> Nothing

{- | Stages 1B and 2B cannot be defeated unless Mugash is attached to a hero; in Nightmare
mode, nor unless Edge of Fangorn is in the victory display, as the setup card reads.
-}
canDefeatStage :: HasGame m => m Bool
canDefeatStage = do
  nightmare <- inNightmareMode
  edge <- getsGame (any ((== toCardCode Locations.edgeOfFangorn) . toCardCode) . (.victoryDisplay) . toAttrs . (.scenario))
  (&& (edge || not nightmare)) <$> mugashOnHero

-- | "Shuffle Mugash into the encounter deck", from play or the encounter discard pile.
shuffleMugashIntoEncounterDeck :: ReverseQueue m => m ()
shuffleMugashIntoEncounterDeck =
  mugash >>= \case
    Just o -> shuffleIntoEncounterDeck (toId o)
    Nothing -> shuffleEncounterDiscardIntoDeck (cardIs Objectives.mugashObjective)

-- | The player searches the encounter deck and discard pile for a Huorn enemy and puts it
-- into play engaged with him, as The Forest's Malice reads; the scenario searches.
searchForHuornEngaged :: ReverseQueue m => PlayerId -> m ()
searchForHuornEngaged pid = push $ ScenarioSpecific "intoFangorn.huornEngaged" (toJSON pid)

-- | The player chooses an enemy or objective in the encounter discard pile, reveals it and
-- adds it to the staging area, as The Angry Forest reads; the scenario offers them.
revealFromDiscard :: ReverseQueue m => PlayerId -> m ()
revealFromDiscard pid = push $ ScenarioSpecific "intoFangorn.revealFromDiscard" (toJSON pid)
