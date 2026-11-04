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
card of the quest deck. Defeated, the stage reveals the top card side 2A as the next stage
(rule sheet), which then turns over at once: the flip follows the advance.
-}
instance RunMessage ForebodingDarkness where
  runMessage msg q@(ForebodingDarkness attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      top <- getsGame (take 1 . (.questDeck) . toAttrs . (.scenario))
      for_ top \next -> push $ Flip (CardIdTarget next.id)
      pure q
    _ -> ForebodingDarkness <$> runExploringGundabad defeated msg attrs
   where
    defeated = do
      top <- getsGame (take 2 . (.questDeck) . toAttrs . (.scenario))
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "forebodingDarkness.bottom"
        causedBy attrs
        showing (map EncounterCard top)
        targets top (putOnBottomOfQuestDeck . EncounterCard)
