module LordOfTheRings.Quest.Cards.EscapeToTheQuays (escapeToTheQuays) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (scrollHolder, scrollOnHero)

newtype EscapeToTheQuays = EscapeToTheQuays QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeToTheQuays :: QuestCard EscapeToTheQuays
escapeToTheQuays = quest EscapeToTheQuays Cards.escapeToTheQuays

-- | Enemies cannot be optionally engaged.
instance HasModifiersFor EscapeToTheQuays where
  getModifiersFor (EscapeToTheQuays a) = modified a GameTarget [CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: The first enemy revealed from the encounter deck each round makes an immediate
-- attack against the player who controls Alcaron's Scroll from the staging area.
instance HasAbilities EscapeToTheQuays where
  getAbilities (EscapeToTheQuays a) =
    [limited (GroupLimit PerRound 1) $ mkAbility a 1 $ forced $ EnemyRevealedWindow After]

-- | When Revealed: Each player searches the encounter deck and discard pile for 1 enemy
-- and adds it to the staging area.
-- The players cannot defeat this stage unless Alcaron's Scroll is attached to a hero. If
-- the players defeat this stage, they have won the game.
instance RunMessage EscapeToTheQuays where
  runMessage msg q@(EscapeToTheQuays attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      EscapeToTheQuays <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> searchEncounterDeckAndDiscard p attrs (CardWithType EnemyType)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      holder <- scrollHolder
      for_ holder \p -> for_ (take 1 [eid | Window _ (Revealed (EnemyTarget eid)) <- ws]) (immediateAttack p)
      pure q
    _ -> EscapeToTheQuays <$> runConditionalStage scrollOnHero msg attrs
