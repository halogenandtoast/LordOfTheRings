module LordOfTheRings.Quest.Cards.FightingInTheTower (fightingInTheTower) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTowerOfCirithUngol.Helpers (orcsFight, resolveOrcsFight)
import LordOfTheRings.Trait (Trait (RingBearer))

newtype FightingInTheTower = FightingInTheTower QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fightingInTheTower :: QuestCard FightingInTheTower
fightingInTheTower = quest FightingInTheTower Cards.fightingInTheTower

-- | Forced: At the beginning of the quest phase, discard cards from the top of the
-- encounter deck until an enemy is discarded. If there is a copy of that enemy in the
-- staging area, discard that copy as well. If there is not, add the discarded enemy to the
-- staging area.
instance HasAbilities FightingInTheTower where
  getAbilities (FightingInTheTower a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

topmostChamberDefeated :: QueueT Message GameT Bool
topmostChamberDefeated = getsGame (any ((== toCardCode Locations.topmostChamber) . toCardCode) . (.victoryDisplay) . toAttrs . (.scenario))

{- | When Revealed: Shuffle the encounter discard pile into the encounter deck and discard
cards from the top until X enemies are discarded, where X is the number of players in the
game. Add each enemy discarded by this effect to the staging area.

This stage cannot be defeated unless Topmost Chamber is in the victory display. When the
Ring-bearer is free of encounters, the first player gains control of it: the hero is
rescued as Topmost Chamber leaves play, under its owner's control, given to the first
player should he be another.
-}
instance RunMessage FightingInTheTower where
  runMessage msg q@(FightingInTheTower attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      FightingInTheTower <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    -- the deck is read once it is shuffled
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType EnemyType)
      pure q
    -- the Forced effect's, read back on behalf of the game
    DiscardedTopOfEncounterDeck (isSource attrs -> True) GameTarget cards -> do
      resolveOrcsFight cards
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType EnemyType) cards
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      orcsFight attrs GameTarget
      pure q
    RescueCharacter (HeroTarget hid) | attrs.flipped -> do
      -- still guarded as it is rescued: no matcher reaches it yet
      hero <- getHero hid
      when (RingBearer `member` cdCardTraits (toCardDef hero)) do
        lead <- getLead
        when (hero.owner /= lead) $ push $ ChangeHeroController hid lead
      FightingInTheTower <$> runConditionalStage topmostChamberDefeated msg attrs
    _ -> FightingInTheTower <$> runConditionalStage topmostChamberDefeated msg attrs
