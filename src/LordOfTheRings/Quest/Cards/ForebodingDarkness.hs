module LordOfTheRings.Quest.Cards.ForebodingDarkness (forebodingDarkness) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype ForebodingDarkness = ForebodingDarkness QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

forebodingDarkness :: QuestCard ForebodingDarkness
forebodingDarkness = quest ForebodingDarkness Cards.forebodingDarkness

-- | Each player cannot play more than 1 card each phase.
instance HasModifiersFor ForebodingDarkness where
  getModifiersFor (ForebodingDarkness a) = when a.flipped do
    effects <- getsGame gameEffects
    players <- getRemainingPlayers
    for_ players \p ->
      when (or [PlayedACardThisPhase `elem` effectModifiers e | e <- effects, effectTarget e == PlayerTarget p])
        $ modified a (PlayerTarget p) [CannotPlay AnyCard]

instance HasAbilities ForebodingDarkness where
  getAbilities (ForebodingDarkness a) = exploringGundabad a <> bypass a

{- | Forced: When this stage is defeated, the first player looks at the top 2 cards of the
quest deck and chooses 1 to place on the bottom of the quest deck. Then, flip over the top
card of the quest deck.
The Forced effect resolves as the stage is defeated, before the next stage is revealed: the
top card turns over in the quest deck, so the next stage comes out on its B side, and its
side 2A, facedown, is never revealed nor its "When Revealed: Return Dagnir to the staging
area" resolved. The rule sheet's "it causes the next stage 2A to be revealed" gives way to the
card (Golden Rule, FAQ 1.00); compare The Side Door, which says to "advance to the top stage
2A".
-}
instance RunMessage ForebodingDarkness where
  runMessage msg (ForebodingDarkness attrs) = ForebodingDarkness <$> runExploringGundabad defeated msg attrs
   where
    defeated = do
      top <- getsGame (take 2 . (.questDeck) . toAttrs . (.scenario))
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "forebodingDarkness.bottom"
        causedBy attrs
        showing (map EncounterCard top)
        targets top \chosen -> do
          putOnBottomOfQuestDeck (EncounterCard chosen)
          -- with 1 card in the quest deck, it is on top again
          flipStage $ CardIdTarget $ maybe chosen.id (.id) (find (/= chosen) top)
